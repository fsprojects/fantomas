module internal Fantomas.Trivia

open Fantomas.AstTransformer
open FSharp.Compiler.Ast
open Fantomas
open Fantomas.TriviaTypes

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

let private mapNodeToTriviaNode (node: Node) =
    node.Range
    |> Option.map (fun range ->
        { Type = MainNode
          CommentsAfter = []
          CommentsBefore = []
          NewlinesBefore = 0
          Range = range }
    )

let private updateTriviaNode lens (triviaNodes: TriviaNode list) triviaNode =
    match triviaNode with
    | Some tNode ->
        triviaNodes
        |> List.map (fun tn -> if tn = tNode then lens tn else tn)
    | None -> triviaNodes

let private addTriviaToTriviaNode (triviaNodes: TriviaNode list) trivia =
    match trivia with
    | { Item = Comment(LineCommentOnSingleLine(_) as comment); Range = range } ->
        findFirstNodeAfterLine triviaNodes range.StartLine
        |> updateTriviaNode (fun tn -> { tn with CommentsBefore = tn.CommentsBefore @ [comment] }) triviaNodes

    | { Item = Comment(LineCommentAfterSourceCode(_) as comment); Range = range } ->
        findLastNodeOnLine triviaNodes range.EndLine
        |> updateTriviaNode (fun tn -> { tn with CommentsAfter = tn.CommentsAfter @ [comment] }) triviaNodes

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
    
let getMainNode index (ts: TriviaNode list) =
    ts |> List.skip index |> List.tryFind (fun t -> t.Type = TriviaNodeType.MainNode)