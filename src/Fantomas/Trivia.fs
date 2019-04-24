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

let private findInChildren fn (node: Node) =
    node.Childs
    |> List.choose fn
    |> List.tryHead

let private findFirstNodeOnLine lineNumber (nodes: Node list) : Node option =
    nodes
    |> List.filter (fun n ->
        match n.Range with
        | Some r -> r.StartLine = lineNumber
        | _ -> false
    )
    |> List.sortBy (fun { Range = r } ->
        match r with
        | Some range -> range.StartCol
        | None -> -1
    )
    |> List.tryHead
    
let private findFirstNodeAfterLine lineNumber (nodes: Node list) : Node option =
    nodes
    |> List.filter (fun n ->
        match n.Range with
        | Some r -> r.StartLine > lineNumber
        | _ -> false
    )
    |> List.sortBy (fun { Range = r; Childs = children } ->
        match r with
        | Some range -> range.StartCol, - (List.length children)
        | None -> -1, 0
    )
    |> List.tryHead


let private findLastNodeOnLine lineNumber (nodes: Node list) : Node option =
    nodes
    |> List.filter (fun n ->
        match n.Range with
        | Some r -> r.EndLine = lineNumber || r.StartLine = lineNumber
        | _ -> false
    )
    |> List.sortByDescending (fun { Range = r } ->
        match r with
        | Some range -> range.EndCol, range.StartCol
        | None -> -1, -1
    )
    |> List.tryHead

let private mapTriviaToTriviaNode nodeList trivia =
    let createCommentBeforeFirstNodeAfter startLine lineComment =
        findFirstNodeAfterLine startLine nodeList
        |> Option.map (fun nextNodeUnder ->
            let t = { Type = MainNode
                      NewlinesBefore = 0
                      CommentsBefore = [LineCommentOnSingleLine(lineComment)]
                      CommentsAfter = [] }
            (nextNodeUnder.FsAstNode, t))
    
    match trivia with
    | { Item = Comment(LineCommentOnSingleLine(lineComment)); Range = range } ->
        // A comment that start at the begin of a line, no other source code is before it on that line
        createCommentBeforeFirstNodeAfter range.StartLine lineComment
        
    | { Item = Comment(LineCommentAfterSourceCode(lineComment)); Range = range } ->
        findLastNodeOnLine (range.StartLine) nodeList
        |> Option.filter (fun lastNodeOnLine ->
            lastNodeOnLine.Range |> Option.exists (fun r -> r.EndLine <= range.StartLine && r.EndCol <= range.StartColumn))
        |> Option.map (fun lastNodeOnLine ->
            let t = { Type = MainNode
                      NewlinesBefore = 0
                      CommentsAfter = [LineCommentAfterSourceCode(lineComment)]
                      CommentsBefore = [] }
            (lastNodeOnLine.FsAstNode, t))
        |> Option.orElseWith (fun () -> createCommentBeforeFirstNodeAfter range.StartLine lineComment)
        
    | { Item = Comment(LineCommentAfterLeftBrace(lineComment)); Range = range } ->
        findFirstNodeOnLine range.StartLine nodeList
        |> Option.map (fun node ->
            let t= { Type = LeftBrace
                     NewlinesBefore = 0
                     CommentsBefore = []
                     CommentsAfter = [LineCommentAfterLeftBrace(lineComment)] }

            (node.FsAstNode, t)
        )
        
    | { Item = Newline; Range = range } ->
        findFirstNodeOnLine (range.StartLine + 1) nodeList // TODO: this approach does not work if multiple newlines are in place.
        |> Option.map (fun nextNodeUnder ->
            let t = { Type = MainNode
                      NewlinesBefore = 1
                      CommentsBefore = []
                      CommentsAfter = [] }
            (nextNodeUnder.FsAstNode, t)
        )

    | _ -> None

let collectTrivia tokens (ast: ParsedInput) =
    // Extra stuff we need is already captured and has regions
    // Now we only need to figure out what to place in what trivia node.
    let trivias = TokenParser.getTriviaFromTokens tokens
    
    match ast with
    | ParsedInput.ImplFile (ParsedImplFileInput.ParsedImplFileInput(_, _, _, _, hs, mns, _)) ->
        let node = Fantomas.AstTransformer.astToNode (mns |> List.collect (function (SynModuleOrNamespace(ats, px, ao, s, mds, isRecursive, isModule, _)) -> s))
        let nodeList = flattenNodeToList node

        trivias
        |> List.map (mapTriviaToTriviaNode nodeList)
        |> List.choose id
        |> List.groupBy fst
        |> List.map (fun (fsAstNode,g) ->
            let triviaNodes =
                g |> List.map snd
                |> List.groupBy (fun t -> t.Type)
                |> List.collect (
                    function
                    | (MainNode, xs) ->
                         [{ Type = MainNode
                            CommentsBefore = xs |> List.collect (fun x -> x.CommentsBefore)
                            CommentsAfter = xs |> List.collect (fun x -> x.CommentsAfter)
                            NewlinesBefore = xs |> Seq.sumBy (fun x -> x.NewlinesBefore)
                         }]
                    | (LeftBrace, xs) ->
                         [{ Type = LeftBrace
                            CommentsBefore = xs |> List.collect (fun x -> x.CommentsBefore)
                            CommentsAfter = xs |> List.collect (fun x -> x.CommentsAfter)
                            NewlinesBefore = xs |> Seq.sumBy (fun x -> x.NewlinesBefore)
                         }]
                    )
                
            fsAstNode, triviaNodes
        )
        |> refDict

    | _ -> Seq.empty |> refDict
    
let getMainNode index (ts: TriviaNode list) =
    ts |> List.skip index |> List.tryFind (fun t -> t.Type = TriviaNodeType.MainNode)
    
let getLeftBraceNode index (ts: TriviaNode list) =
    ts |> List.skip index |> List.tryFind (fun t -> t.Type = TriviaNodeType.LeftBrace)