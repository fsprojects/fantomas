module internal Fantomas.Trivia

open Fantomas.AstTransformer
open FSharp.Compiler.Ast
open Fantomas
open Fantomas.TriviaTypes

let refDict xs =
    let d = System.Collections.Generic.Dictionary(HashIdentity.Reference)
    xs |> Seq.iter d.Add
    d

let rec private flattenNodeToList (node: Node) =
    [ yield node
      yield! (node.Childs |> List.map flattenNodeToList |> List.collect id) ]

//let private findInChildren fn (node: Node) =
//    node.Childs
//    |> List.choose fn
//    |> List.tryHead
//
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
//
//
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
        

//let private mapTriviaToTriviaNode nodeList trivia =
//    let createCommentBeforeFirstNodeAfter startLine lineComment =
//        findFirstNodeAfterLine startLine nodeList
//        |> Option.map (fun nextNodeUnder ->
//            let t = { Type = MainNode
//                      NewlinesBefore = 0
//                      CommentsBefore = [LineCommentOnSingleLine(lineComment)]
//                      CommentsAfter = [] }
//            (nextNodeUnder.FsAstNode, t))
//    
//    match trivia with
//    | { Item = Comment(LineCommentOnSingleLine(lineComment)); Range = range } ->
//        // A comment that start at the begin of a line, no other source code is before it on that line
//        createCommentBeforeFirstNodeAfter range.StartLine lineComment
//        
//    | { Item = Comment(LineCommentAfterSourceCode(lineComment)); Range = range } ->
//        findLastNodeOnLine (range.StartLine) nodeList
//        |> Option.filter (fun lastNodeOnLine ->
//            lastNodeOnLine.Range |> Option.exists (fun r -> r.EndLine <= range.StartLine && r.EndCol <= range.StartColumn))
//        |> Option.map (fun lastNodeOnLine ->
//            let t = { Type = MainNode
//                      NewlinesBefore = 0
//                      CommentsAfter = [LineCommentAfterSourceCode(lineComment)]
//                      CommentsBefore = [] }
//            (lastNodeOnLine.FsAstNode, t))
//        |> Option.orElseWith (fun () -> createCommentBeforeFirstNodeAfter range.StartLine lineComment)
//        
//    | { Item = Comment(LineCommentAfterLeftBrace(lineComment)); Range = range } ->
//        findFirstNodeOnLine range.StartLine nodeList
//        |> Option.map (fun node ->
//            let t= { Type = LeftBrace
//                     NewlinesBefore = 0
//                     CommentsBefore = []
//                     CommentsAfter = [LineCommentAfterLeftBrace(lineComment)] }
//
//            (node.FsAstNode, t)
//        )
//        
//    | { Item = Newline; Range = range } ->
//        findFirstNodeOnLine (range.StartLine + 1) nodeList // TODO: this approach does not work if multiple newlines are in place.
//        |> Option.map (fun nextNodeUnder ->
//            let t = { Type = MainNode
//                      NewlinesBefore = 1
//                      CommentsBefore = []
//                      CommentsAfter = [] }
//            (nextNodeUnder.FsAstNode, t)
//        )
//
//    | _ -> None

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

let collectTrivia tokens (ast: ParsedInput) =
    match ast with
    | ParsedInput.ImplFile (ParsedImplFileInput.ParsedImplFileInput(_, _, _, _, hs, mns, _)) ->
        let node = Fantomas.AstTransformer.astToNode (mns |> List.collect (function (SynModuleOrNamespace(ats, px, ao, s, mds, isRecursive, isModule, _)) -> s))
        let triviaNodesFromAST =
            flattenNodeToList node
            |> List.map mapNodeToTriviaNode
            |> List.choose id
        let triviaNodesFromTokens = TokenParser.getTriviaNodesFromTokens tokens
        let triviaNodes = triviaNodesFromAST @ triviaNodesFromTokens
        
        // Extra stuff we need is already captured and has regions
        // Now we only need to figure out what to place in what trivia node.
        let trivias = TokenParser.getTriviaFromTokens tokens
        
        List.fold addTriviaToTriviaNode triviaNodes trivias
        |> fun x -> x
        |> List.filter (triviaNodeIsNotEmpty)

        //failwith "not implemented"

        (*
        1. Collect TriviaNode from tokens and AST
        2. Collect TriviaContent from tokens
        3. Merge trivias with triviaNodes
        4. genTrivia should use ranges to identify what extra content should be added from what triviaNode
        *)
        
//        trivias
//        |> List.map (mapTriviaToTriviaNode nodeList)
//        |> List.choose id
//        |> List.groupBy fst
//        |> List.map (fun (fsAstNode,g) ->
//            let triviaNodes =
//                g |> List.map snd
//                |> List.groupBy (fun t -> t.Type)
//                |> List.collect (
//                    function
//                    | (MainNode, xs) ->
//                         [{ Type = MainNode
//                            CommentsBefore = xs |> List.collect (fun x -> x.CommentsBefore)
//                            CommentsAfter = xs |> List.collect (fun x -> x.CommentsAfter)
//                            NewlinesBefore = xs |> Seq.sumBy (fun x -> x.NewlinesBefore)
//                         }]
//                    | (LeftBrace, xs) ->
//                         [{ Type = LeftBrace
//                            CommentsBefore = xs |> List.collect (fun x -> x.CommentsBefore)
//                            CommentsAfter = xs |> List.collect (fun x -> x.CommentsAfter)
//                            NewlinesBefore = xs |> Seq.sumBy (fun x -> x.NewlinesBefore)
//                         }]
//                    )
//                
//            fsAstNode, triviaNodes
//        )
//        |> refDict

    | _ -> [] // Seq.empty |> refDict
    
let getMainNode index (ts: TriviaNode list) =
    ts |> List.skip index |> List.tryFind (fun t -> t.Type = TriviaNodeType.MainNode)