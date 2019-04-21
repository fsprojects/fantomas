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


let private findLastNodeOnLine lineNumber (nodes: Node list) : Node option =
    nodes
    |> List.filter (fun n ->
        match n.Range with
        | Some r -> r.EndLine = lineNumber && List.isEmpty n.Childs
        | _ -> false
    )
    |> List.sortByDescending (fun { Range = r } ->
        match r with
        | Some range -> range.EndCol
        | None -> -1
    )
    |> List.tryHead

let private mapTriviaToTriviaNode nodeList trivia =
    match trivia with
    | { Item = Comment(LineCommentOnSingleLine(lineComment)); Range = range } ->
        // A comment that start at the begin of a line, no other source code is before it on that line
        let nextNodeUnder = findFirstNodeOnLine (range.StartLine + 1) nodeList
        Option.toList nextNodeUnder
        |> List.map (fun n ->
            let t = { Type = MainNode
                      NewlinesBefore = 0
                      CommentsBefore = [LineCommentOnSingleLine(lineComment)]
                      CommentsAfter = [] }
            (n.FsAstNode, [t])
        )
        
    | { Item = Comment(LineCommentAfterSourceCode(lineComment)); Range = range } ->
        let lastNodeOnLine = findLastNodeOnLine (range.StartLine) nodeList
        Option.toList lastNodeOnLine
        |> List.map (fun n ->
            let t = { Type = MainNode
                      NewlinesBefore = 0
                      CommentsAfter = [LineCommentAfterSourceCode(lineComment)]
                      CommentsBefore = [] }
            (n.FsAstNode, [t])
        )
        
    | { Item = Newline; Range = range } ->
        let nextNodeUnder = findFirstNodeOnLine (range.StartLine + 1) nodeList // TODO: this approach does not work if multiple newlines are in place.
        Option.toList nextNodeUnder
        |> List.map (fun n ->
            let t = { Type = MainNode
                      NewlinesBefore = 1
                      CommentsBefore = []
                      CommentsAfter = [] }
            (n.FsAstNode, [t])
        )
        
    | _ -> []

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
        |> List.collect id // TODO: at this point it is not a perfect dictionary as multiple trivia can be linked to a same AST node
        |> refDict

    | _ -> Seq.empty |> refDict
    
let getMainNode index (ts: TriviaNode list) =
    ts |> List.skip index |> List.tryFind (fun t -> t.Type = TriviaNodeType.MainNode)