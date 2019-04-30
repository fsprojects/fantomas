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

let private mapNodeToTriviaNode (node: Node) =
    node.Range
    |> Option.map (fun range ->
        { Type = MainNode(node.Type)
          CommentsAfter = []
          CommentsBefore = []
          NewlinesBefore = 0
          Range = range }
    )
    
let private commentIsAfterLastTriviaNode (triviaNodes: TriviaNode list) (range: range) =
    triviaNodes
    |> List.filter isMainNodeButNotAModule
    |> List.forall (fun tn -> tn.Range.EndLine < range.StartLine)

let private updateTriviaNode lens (triviaNodes: TriviaNode list) triviaNode =
    match triviaNode with
    | Some tNode ->
        triviaNodes
        |> List.map (fun tn -> if tn = tNode then lens tn else tn)
    | None -> triviaNodes

let private addTriviaToTriviaNode (triviaNodes: TriviaNode list) trivia =
    match trivia with
    | { Item = Comment(LineCommentOnSingleLine(lineComment) as comment); Range = range } when (commentIsAfterLastTriviaNode triviaNodes range) ->
        // Comment on is on its own line after all Trivia nodes, most likely at the end of a module
        findLastNode triviaNodes
        |> updateTriviaNode (fun tn ->
            match tn.CommentsAfter with
            | [] -> // make sure that comment ends up under the printed code
                LineCommentOnSingleLine(sprintf "%s%s" Environment.NewLine lineComment)
                |> List.singleton
                |> fun ca -> { tn with CommentsAfter = ca }
            | _ ->
                { tn with CommentsAfter = List.appendItem tn.CommentsAfter comment }
        ) triviaNodes

    | { Item = Comment(LineCommentOnSingleLine(_) as comment); Range = range } ->
        findFirstNodeAfterLine triviaNodes range.StartLine
        |> updateTriviaNode (fun tn -> { tn with CommentsBefore = List.appendItem  tn.CommentsBefore comment }) triviaNodes

    | { Item = Comment(LineCommentAfterSourceCode(_) as comment); Range = range } ->
        findLastNodeOnLine triviaNodes range.EndLine
        |> updateTriviaNode (fun tn -> { tn with CommentsAfter = List.appendItem tn.CommentsAfter comment }) triviaNodes

    | { Item = Newline; Range = range } ->
        findFirstNodeOnLine triviaNodes (range.StartLine + 1) // TODO: this approach does not work if multiple newlines are in place.
        |> updateTriviaNode (fun tn -> { tn with NewlinesBefore = tn.NewlinesBefore + 1 }) triviaNodes

    | _ ->
        triviaNodes

let private triviaNodeIsNotEmpty triviaNode =
    triviaNode.NewlinesBefore > 0 || not(List.isEmpty triviaNode.CommentsAfter) || not(List.isEmpty triviaNode.CommentsBefore)

(*
    1. Collect TriviaNode from tokens and AST
    2. Collect TriviaContent from tokens
    3. Merge trivias with triviaNodes
    4. genTrivia should use ranges to identify what extra content should be added from what triviaNode
*)
let collectTrivia tokens (ast: ParsedInput) =
    match ast with
    | ParsedInput.ImplFile (ParsedImplFileInput.ParsedImplFileInput(_, _, _, _, hs, mns, _)) ->
        let node = Fantomas.AstTransformer.astToNode mns
        let triviaNodesFromAST =
            flattenNodeToList node
            |> List.map mapNodeToTriviaNode
            |> List.choose id
        let triviaNodesFromTokens = TokenParser.getTriviaNodesFromTokens tokens
        let triviaNodes = triviaNodesFromAST @ triviaNodesFromTokens
        
        let trivias = TokenParser.getTriviaFromTokens tokens
        
        List.fold addTriviaToTriviaNode triviaNodes trivias
        |> fun x -> x
        |> List.filter (triviaNodeIsNotEmpty) // only keep nodes where something special needs to happen.

    | _ -> []