module internal Fantomas.Trivia

open System
open Fantomas.AstTransformer
open FSharp.Compiler.Ast
open Fantomas
open Fantomas.TriviaTypes
open FSharp.Compiler.Range

let private isMainNodeButNotAModule (node: TriviaNode) =
    match node.Type with
    | MainNode(t) when (t <> "SynModuleOrNamespace") -> true
    | _ -> false
    
let isMainNode (node: TriviaNode) =
    match node.Type with
    | MainNode(_) -> true
    | _ -> false

let rec private flattenNodeToList (node: Node) =
    [ yield node
      yield! (node.Childs |> List.map flattenNodeToList |> List.collect id) ]

let private findFirstNodeOnLine (nodes: TriviaNode list) lineNumber : TriviaNode option =
    nodes
    |> List.filter (fun { Range =r  } -> r.StartLine = lineNumber)
    |> List.sortBy (fun { Range = r } -> r.StartColumn)
    |> List.tryHead
    
let private findFirstNodeAfterLine (nodes: TriviaNode list) lineNumber : TriviaNode option =
    nodes
    |> List.filter (fun n -> n.Range.StartLine > lineNumber)
    |> fun filteredNodes ->
        match filteredNodes with
        | moduleAndOpens when (List.forall (fun t -> t.Type = MainNode("SynModuleOrNamespace") || t.Type = MainNode("SynModuleDecl.Open")) moduleAndOpens) ->
            moduleAndOpens
            |> List.filter (fun t -> t.Type = MainNode("SynModuleDecl.Open"))
            |> List.sortBy (fun t -> t.Range.StartLine)
            |> List.tryHead
        | _ ->
            filteredNodes
            |> List.sortBy (fun { Range = r } -> r.StartLine, r.StartColumn)
            |> List.tryHead

let private findLastNodeOnLine (nodes: TriviaNode list) lineNumber : TriviaNode option =
    nodes
    |> List.filter (fun { Range = r } -> r.EndLine = lineNumber)
    |> List.sortByDescending (fun { Range = r } -> r.EndColumn, r.StartColumn)
    |> List.tryHead

let private findLastNode (nodes: TriviaNode list) : TriviaNode option =
    match nodes with
    | [] -> None
    | nodes ->
        nodes
        |> List.filter isMainNodeButNotAModule
        |> List.maxBy (fun tn -> tn.Range.EndLine)
        |> Some

let private findNodeOnLineAndColumn (nodes: TriviaNode list) line column =
    nodes
    |> List.tryFindBack (fun { Range = range } -> range.StartLine = line && range.StartColumn = column)

let private mapNodeToTriviaNode (node: Node) =
    node.Range
    |> Option.map (fun range ->
        { Type = MainNode(node.Type)
          ContentAfter = []
          ContentBefore = []
          Range = range }
    )
    
let private commentIsAfterLastTriviaNode (triviaNodes: TriviaNode list) (range: range) =
    triviaNodes
    |> List.filter isMainNodeButNotAModule
    |> List.forall (fun tn -> tn.Range.EndLine < range.StartLine)

let private updateTriviaNode lens (triviaNodes: TriviaNode list) triviaNode =
    match triviaNode with
    | Some tNode ->
        // There are situations where the same range can be linked to multiple different AST nodes.
        // F.ex a getter and setter in one line.
        // We want to be sure that one node will be projected by the lens function.
        let index =
            triviaNodes
            |> List.findIndex (fun tn -> tn = tNode)
        
        triviaNodes
        |> List.mapi (fun idx tn -> if idx = index then lens tn else tn)
    | None -> triviaNodes

let private addTriviaToTriviaNode (triviaNodes: TriviaNode list) trivia =
    match trivia with
    | { Item = Comment(LineCommentOnSingleLine(lineComment) as comment); Range = range } when (commentIsAfterLastTriviaNode triviaNodes range) ->
        // Comment on is on its own line after all Trivia nodes, most likely at the end of a module
        findLastNode triviaNodes
        |> updateTriviaNode (fun tn ->
            match tn.ContentAfter with
            | [] -> // make sure that comment ends up under the printed code
                LineCommentOnSingleLine(sprintf "%s%s" Environment.NewLine lineComment)
                |> Comment
                |> List.singleton
                |> fun ca -> { tn with ContentAfter = ca }
            | _ ->
                { tn with ContentAfter = List.appendItem tn.ContentAfter (Comment(comment)) }
        ) triviaNodes

    | { Item = Comment(LineCommentOnSingleLine(_) as comment); Range = range } ->
        findFirstNodeAfterLine triviaNodes range.StartLine
        |> updateTriviaNode (fun tn -> { tn with ContentBefore = List.appendItem  tn.ContentBefore (Comment(comment)) }) triviaNodes

    | { Item = Comment(BlockComment(_) as comment); Range = range } ->
        findFirstNodeAfterLine triviaNodes range.StartLine
        |> updateTriviaNode (fun tn -> 
            let newline = if tn.Range.StartLine > range.EndLine then [Newline] else []
            { tn with ContentBefore = tn.ContentBefore @ [Comment(comment)] @ newline }) triviaNodes    

    | { Item = Comment(LineCommentAfterSourceCode(_) as comment); Range = range } ->
        findLastNodeOnLine triviaNodes range.EndLine
        |> updateTriviaNode (fun tn -> { tn with ContentAfter = List.appendItem tn.ContentAfter (Comment(comment)) }) triviaNodes

    | { Item = Newline; Range = range } ->
        findFirstNodeAfterLine triviaNodes range.StartLine // TODO: this approach does not work if multiple newlines are in place.
        |> updateTriviaNode (fun tn -> { tn with ContentBefore = List.appendItem tn.ContentBefore Newline }) triviaNodes
        
    | { Item = Keyword(keyword); Range = range } ->
        findNodeOnLineAndColumn triviaNodes range.StartLine range.StartColumn
        |> updateTriviaNode (fun tn -> { tn with ContentBefore = List.appendItem tn.ContentBefore (Keyword(keyword)) }) triviaNodes

    | _ ->
        triviaNodes

let private triviaNodeIsNotEmpty triviaNode =
    not(List.isEmpty triviaNode.ContentAfter) || not(List.isEmpty triviaNode.ContentBefore)

(*
    1. Collect TriviaNode from tokens and AST
    2. Collect TriviaContent from tokens
    3. Merge trivias with triviaNodes
    4. genTrivia should use ranges to identify what extra content should be added from what triviaNode
*)
let collectTrivia tokens lineCount (ast: ParsedInput) =
    match ast with
    | ParsedInput.ImplFile (ParsedImplFileInput.ParsedImplFileInput(_, _, _, _, _, mns, _)) ->
        let node = Fantomas.AstTransformer.astToNode mns
        let triviaNodesFromAST =
            flattenNodeToList node
            |> List.map mapNodeToTriviaNode
            |> List.choose id
        let triviaNodesFromTokens = TokenParser.getTriviaNodesFromTokens tokens
        let triviaNodes = triviaNodesFromAST @ triviaNodesFromTokens
        
        let trivias = TokenParser.getTriviaFromTokens tokens lineCount
        
        // printfn "%A" trivias

        List.fold addTriviaToTriviaNode triviaNodes trivias
        |> List.filter (triviaNodeIsNotEmpty) // only keep nodes where something special needs to happen.
        // |> fun x -> printfn "%A" x; x

    | _ -> []