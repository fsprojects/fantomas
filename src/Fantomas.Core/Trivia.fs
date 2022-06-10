module internal Fantomas.Core.Trivia

open Microsoft.FSharp.Core.CompilerServices
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Fantomas.Core
open Fantomas.Core.ISourceTextExtensions
open Fantomas.Core.SourceParser
open Fantomas.Core.AstExtensions
open Fantomas.Core.AstTransformer
open Fantomas.Core.TriviaTypes
open Fantomas.Core.FormatConfig

let findFirstNodeAfterLine (nodes: TriviaNodeAssigner list) (lineNumber: int) : TriviaNodeAssigner option =
    nodes
    |> List.tryFind (fun tn -> tn.Range.StartLine > lineNumber)

let findLastNodeOnLine (nodes: TriviaNodeAssigner list) lineNumber : TriviaNodeAssigner option =
    nodes
    |> List.filter (fun tn -> tn.Range.EndLine = lineNumber)
    |> List.sortByDescending (fun tn -> tn.Range.EndColumn, tn.Range.StartColumn)
    |> fun candidates ->
        match candidates with
        | app :: ident :: _ when
            (app.Range.End = ident.Range.End
             && app.Type = SynExpr_App
             && ident.Type = SynExpr_Ident)
            ->
            Some ident
        | h :: _ -> Some h
        | [] -> None

let findLastNode (nodes: TriviaNodeAssigner list) : TriviaNodeAssigner option =
    match nodes with
    | [] -> None
    | nodes ->
        nodes
        |> List.maxBy (fun tn -> tn.Range.EndLine)
        |> Some

let findNodeBeforeLineAndColumn (nodes: TriviaNodeAssigner seq) line column =
    nodes
    |> Seq.tryFindBack (fun tn ->
        let range = tn.Range

        range.StartLine <= line
        && range.StartColumn <= column)

let findNodeBeforeLineFromStart (nodes: TriviaNodeAssigner list) line =
    nodes
    |> List.filter (fun tn -> tn.Range.EndLine < line)
    |> List.sortByDescending (fun tn -> tn.Range.EndLine, -tn.Range.StartColumn)
    |> List.tryFind (fun tn -> tn.Range.StartLine < line)

let findNodeAfterLineAndColumn (nodes: TriviaNodeAssigner seq) line column =
    nodes
    |> Seq.tryFind (fun tn ->
        let range = tn.Range

        (range.StartLine > line)
        || (range.StartLine = line
            && range.StartColumn > column))

let commentIsAfterLastTriviaNode (triviaNodes: TriviaNodeAssigner list) (range: Range) =
    let hasNoNodesAfterRange =
        triviaNodes
        |> Seq.exists (fun tn -> tn.Range.EndLine > range.StartLine)
        |> not

    hasNoNodesAfterRange

// let updateTriviaNode (lens: TriviaNodeAssigner -> unit) (triviaNodes: TriviaNodeAssigner list) triviaNode =
//     match triviaNode with
//     | Some tNode ->
//         // There are situations where the same range can be linked to multiple different AST nodes.
//         // F.ex a getter and setter in one line.
//         // We want to be sure that one node will be projected by the lens function.
//         let index =
//             triviaNodes
//             |> List.findIndex (fun tn -> tn = tNode)
//
//         lens triviaNodes.[index]
//
//         triviaNodes
//     | None -> triviaNodes

let addAllTriviaAsContentAfter (trivia: Trivia list) (singleNode: TriviaNodeAssigner) =
    let contentAfter =
        trivia
        |> List.skipWhile (fun tn -> tn.Item = Newline) // skip leading newlines
        |> List.map (fun tn -> tn.Item)

    { Type = singleNode.Type
      Range = singleNode.Range
      ContentBefore = []
      ContentItself = None
      ContentAfter = contentAfter }
    |> List.singleton

let printTriviaNode (node: TriviaNodeAssigner) : unit =
    let rec visit (level: int) (node: TriviaNodeAssigner) =
        printfn "%s%A: %A" ("".PadRight(level * 2)) node.Type node.Range
        Array.iter (visit (level + 1)) node.Children

    visit 0 node

let rec findNodeWhereRangeFitsIn (root: TriviaNodeAssigner) (range: range) : TriviaNodeAssigner option =
    let doesSelectionFitInNode = RangeHelpers.rangeContainsRange root.Range range

    if not doesSelectionFitInNode then
        None
    else
        // The more specific the node fits the selection, the better
        let betterChildNode =
            root.Children
            |> Array.choose (fun childNode -> findNodeWhereRangeFitsIn childNode range)
            |> Array.tryHead

        match betterChildNode with
        | Some betterChild -> Some betterChild
        | None -> Some root

// let addTriviaToTriviaNode (startOfSourceCode: int) (triviaNodes: TriviaNodeAssigner list) trivia = triviaNodes
// match trivia with
// | { Item = Comment (LineCommentOnSingleLine _ as comment)
//     Range = range } ->
//     let nodeAfterLine = findFirstNodeAfterLine triviaNodes range.StartLine
//
//     match nodeAfterLine with
//     | Some _ ->
//         nodeAfterLine
//         |> updateTriviaNode (fun tn -> tn.ContentBefore.Add(Comment(comment))) triviaNodes
//     | None ->
//         // try and find a node above
//         findNodeBeforeLineFromStart triviaNodes range.StartLine
//         |> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Comment(comment))) triviaNodes
//
// | { Item = Comment (BlockComment (comment, _, _))
//     Range = range } ->
//     let nodeAfter =
//         findNodeAfterLineAndColumn triviaNodes range.StartLine range.StartColumn
//
//     let nodeBefore =
//         findNodeBeforeLineAndColumn triviaNodes range.StartLine range.StartColumn
//
//     match nodeBefore, nodeAfter with
//     | Some nb, Some na when
//         (nb.Range.EndLine < range.StartLine
//          && na.Range.StartLine > range.EndLine)
//         ->
//         Some na
//         |> updateTriviaNode (fun tn -> tn.ContentBefore.Add(Comment(BlockComment(comment, true, true)))) triviaNodes
//     | Some n, _ when n.Range.EndLine = range.StartLine ->
//         Some n
//         |> updateTriviaNode
//             (fun tn -> tn.ContentAfter.Add(Comment(BlockComment(comment, false, false))))
//             triviaNodes
//     | _, Some n ->
//         Some n
//         |> updateTriviaNode
//             (fun tn ->
//                 let newline = tn.Range.StartLine > range.EndLine
//                 tn.ContentBefore.Add(Comment(BlockComment(comment, false, newline))))
//             triviaNodes
//     | Some _, _ when (commentIsAfterLastTriviaNode triviaNodes range) ->
//         findLastNode triviaNodes
//         |> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Comment(BlockComment(comment, true, false)))) triviaNodes
//     | Some n, _ ->
//         Some n
//         |> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Comment(BlockComment(comment, true, false)))) triviaNodes
//     | None, None ->
//         findNodeBeforeLineFromStart triviaNodes range.StartLine
//         |> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Comment(BlockComment(comment, true, true)))) triviaNodes
//
// | { Item = Comment (LineCommentAfterSourceCode _ as comment)
//     Range = range } ->
//     findLastNodeOnLine triviaNodes range.EndLine
//     |> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Comment(comment))) triviaNodes
//
// // Newlines are only relevant if they occur after the first line of source code
// | { Item = Newline; Range = range } when (range.StartLine > startOfSourceCode) ->
//     let nodeAfterLine = findFirstNodeAfterLine triviaNodes range.StartLine
//
//     match nodeAfterLine with
//     | Some _ ->
//         nodeAfterLine
//         |> updateTriviaNode (fun tn -> tn.ContentBefore.Add(Newline)) triviaNodes
//     | None ->
//         // try and find a node above
//         findNodeBeforeLineFromStart triviaNodes range.StartLine
//         |> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Newline)) triviaNodes
//
// | { Item = Directive dc as directive
//     Range = range } ->
//     let nodeAfterLine = findFirstNodeAfterLine triviaNodes range.StartLine
//
//     match nodeAfterLine with
//     | Some _ as node -> updateTriviaNode (fun tn -> tn.ContentBefore.Add(directive)) triviaNodes node
//     | None ->
//         let findNode nodes =
//             findNodeBeforeLineFromStart nodes range.StartLine
//
//         findNode triviaNodes
//         |> updateTriviaNode
//             (fun tn ->
//                 let directive = Directive dc
//                 tn.ContentAfter.Add(directive))
//             triviaNodes
//
// | _ -> triviaNodes

// let triviaNodeIsNotEmpty (triviaNode: TriviaNodeAssigner) =
//     not (Seq.isEmpty triviaNode.ContentAfter)
//     || not (Seq.isEmpty triviaNode.ContentBefore)
//     || Option.isSome triviaNode.ContentItself
//
// let transformNonEmptyNodes (nodes: TriviaNodeAssigner list) : TriviaNode list =
//     nodes
//     |> List.choose (fun tn ->
//         if triviaNodeIsNotEmpty tn then
//             { Type = tn.Type
//               Range = tn.Range
//               ContentBefore = Seq.toList tn.ContentBefore
//               ContentItself = tn.ContentItself
//               ContentAfter = Seq.toList tn.ContentAfter }
//             |> Some
//         else
//             None)

let internal collectTriviaFromDirectives
    (source: ISourceText)
    (directives: ConditionalDirectiveTrivia list)
    (selection: TriviaForSelection option)
    : Trivia list =
    directives
    |> List.map (function
        | ConditionalDirectiveTrivia.If (_, r)
        | ConditionalDirectiveTrivia.Else r
        | ConditionalDirectiveTrivia.EndIf r ->
            let text = (source.GetContentAt r).TrimEnd()
            { Item = Directive text; Range = r })
    |> fun trivia ->
        match selection with
        | None -> trivia
        | Some { Selection = selection } ->
            List.filter (fun t -> RangeHelpers.rangeContainsRange selection t.Range) trivia

let internal collectTriviaFromCodeComments
    (source: ISourceText)
    (codeComments: CommentTrivia list)
    (selection: TriviaForSelection option)
    : Trivia list =
    codeComments
    |> List.map (function
        | CommentTrivia.BlockComment r ->
            let content = source.GetContentAt r
            let startLine = source.GetLineString(r.StartLine - 1)
            let endLine = source.GetLineString(r.EndLine - 1)

            if
                startLine.TrimStart(' ', ';').StartsWith("(*")
                && endLine.TrimEnd(' ', ';').EndsWith("*)")
            then
                { Item = Comment(CommentOnSingleLine(content))
                  Range = r }
            else
                { Item = Comment(BlockComment(source.GetContentAt r, false, false))
                  Range = r }

        | CommentTrivia.LineComment r ->
            let content = source.GetContentAt r
            let line = source.GetLineString(r.StartLine - 1)

            let item =
                Comment(
                    if line.TrimStart(' ', ';').StartsWith("//") then
                        CommentOnSingleLine content
                    else
                        LineCommentAfterSourceCode content
                )

            { Item = item; Range = r })
    |> fun trivia ->
        match selection with
        | None -> trivia
        | Some { Selection = selection } ->
            List.filter (fun t -> RangeHelpers.rangeContainsRange selection t.Range) trivia

let internal collectTriviaFromBlankLines
    (config: FormatConfig)
    (source: ISourceText)
    (triviaNode: TriviaNodeAssigner)
    (codeComments: CommentTrivia list)
    (codeRange: range)
    : Trivia list =
    let fileIndex = triviaNode.Range.FileIndex

    let captureLinesIfMultiline (r: range) =
        if r.StartLine = r.EndLine then
            []
        else
            [ r.StartLine .. r.EndLine ]

    let multilineStringsLines =
        let rec visit (node: TriviaNodeAssigner) (finalContinuation: int list -> int list) =
            let continuations: ((int list -> int list) -> int list) list =
                Array.toList node.Children |> List.map visit

            let currentLines =
                match node.Type with
                | SynConst_String
                | SynConst_Bytes
                | SynInterpolatedStringPart_String -> captureLinesIfMultiline node.Range
                | _ -> []

            let finalContinuation (lines: int list list) : int list =
                List.collect id (currentLines :: lines)
                |> finalContinuation

            Continuation.sequence continuations finalContinuation

        visit triviaNode id

    let blockCommentLines =
        codeComments
        |> List.collect (function
            | CommentTrivia.BlockComment r -> captureLinesIfMultiline r
            | CommentTrivia.LineComment _ -> [])

    let ignoreLines =
        Set(
            seq {
                yield! multilineStringsLines
                yield! blockCommentLines
            }
        )

    let min = codeRange.StartLine - 1
    let max = codeRange.EndLine - 1

    (min, [ min..max ])
    ||> List.chooseState (fun count idx ->
        if ignoreLines.Contains(idx + 1) then
            0, None
        else
            let line = source.GetLineString(idx)

            if String.isNotNullOrWhitespace line then
                0, None
            else
                let range =
                    let p = Position.mkPos (idx + 1) 0
                    Range.mkFileIndexRange fileIndex p p

                if count < config.KeepMaxNumberOfBlankLines then
                    (count + 1), Some { Item = Newline; Range = range }
                else
                    count, None)

/// The trivia is not a part of the tree
/// Either assign it on top of below the root node
let triviaBeforeOrAfterEntireTree (rootNode: TriviaNodeAssigner) (trivia: Trivia) : TriviaInstruction =
    let isBefore = trivia.Range.EndLine < rootNode.Range.StartLine

    let trivia =
        match trivia.Item with
        | Comment (BlockComment (commentText, _, _)) ->
            { trivia with Item = Comment(BlockComment(commentText, false, true)) }
        | _ -> trivia

    { Trivia = trivia
      Type = rootNode.Type
      Range = rootNode.Range
      AddBefore = isBefore }

/// Try to put the trivia on top of the closest node
/// If that didn't work put it after the last node
let simpleTriviaToTriviaInstruction (containerNode: TriviaNodeAssigner) (trivia: Trivia) : TriviaInstruction option =
    containerNode.Children
    |> Array.tryFind (fun node -> node.Range.StartLine > trivia.Range.StartLine)
    |> Option.map (fun node ->
        { Trivia = trivia
          Type = node.Type
          Range = node.Range
          AddBefore = true })
    |> Option.orElseWith (fun () ->
        Array.tryLast containerNode.Children
        |> Option.map (fun node ->
            { Trivia = trivia
              Type = node.Type
              Range = node.Range
              AddBefore = false }))

let lineCommentAfterSourceCodeToTriviaInstruction
    (containerNode: TriviaNodeAssigner)
    (trivia: Trivia)
    : TriviaInstruction option =
    let lineNumber = trivia.Range.StartLine

    containerNode.Children
    |> Array.filter (fun node -> node.Range.EndLine = lineNumber)
    |> Array.sortByDescending (fun node -> node.Range.StartColumn)
    |> Array.tryHead
    |> Option.map (fun node ->
        { Trivia = trivia
          Type = node.Type
          Range = node.Range
          AddBefore = false })

let blockCommentToTriviaInstruction (containerNode: TriviaNodeAssigner) (trivia: Trivia) : TriviaInstruction option =
    let nodeAfter =
        findNodeAfterLineAndColumn containerNode.Children trivia.Range.StartLine trivia.Range.StartColumn

    let nodeBefore =
        findNodeBeforeLineAndColumn containerNode.Children trivia.Range.StartLine trivia.Range.StartColumn

    let triviaWith newlineBefore newlineAfter =
        match trivia with
        | { Item = Comment (BlockComment (content, _, _)) } ->
            { trivia with Item = Comment(BlockComment(content, newlineBefore, newlineAfter)) }
        | _ -> trivia

    match nodeBefore, nodeAfter with
    | Some nb, None when nb.Range.EndLine = trivia.Range.StartLine ->
        Some
            { Trivia = triviaWith false false
              Type = nb.Type
              Range = nb.Range
              AddBefore = false }
    | Some nb, Some na when
        (nb.Range.EndLine < trivia.Range.StartLine
         && na.Range.StartLine > trivia.Range.EndLine)
        ->
        Some
            { Trivia = triviaWith true true
              Type = na.Type
              Range = na.Range
              AddBefore = true }

    | Some nb, _ when nb.Range.EndLine = trivia.Range.StartLine ->
        Some
            { Trivia = triviaWith false false
              Type = nb.Type
              Range = nb.Range
              AddBefore = false }
    | _ -> None

//     match nodeBefore, nodeAfter with
//     | Some nb, Some na when
//         (nb.Range.EndLine < range.StartLine
//          && na.Range.StartLine > range.EndLine)
//         ->
//         Some na
//         |> updateTriviaNode (fun tn -> tn.ContentBefore.Add(Comment(BlockComment(comment, true, true)))) triviaNodes
//     | Some n, _ when n.Range.EndLine = range.StartLine ->
//         Some n
//         |> updateTriviaNode
//             (fun tn -> tn.ContentAfter.Add(Comment(BlockComment(comment, false, false))))
//             triviaNodes
//     | _, Some n ->
//         Some n
//         |> updateTriviaNode
//             (fun tn ->
//                 let newline = tn.Range.StartLine > range.EndLine
//                 tn.ContentBefore.Add(Comment(BlockComment(comment, false, newline))))
//             triviaNodes
//     | Some _, _ when (commentIsAfterLastTriviaNode triviaNodes range) ->
//         findLastNode triviaNodes
//         |> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Comment(BlockComment(comment, true, false)))) triviaNodes
//     | Some n, _ ->
//         Some n
//         |> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Comment(BlockComment(comment, true, false)))) triviaNodes
//     | None, None ->
//         findNodeBeforeLineFromStart triviaNodes range.StartLine
//         |> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Comment(BlockComment(comment, true, true)))) triviaNodes

let mapTriviaToTriviaInstruction (rootNode: TriviaNodeAssigner) (trivia: Trivia) : TriviaInstruction option =
    let smallestNodeThatContainsTrivia = findNodeWhereRangeFitsIn rootNode trivia.Range

    match smallestNodeThatContainsTrivia with
    | None -> Some(triviaBeforeOrAfterEntireTree rootNode trivia)
    | Some parentNode ->
        match trivia.Item with
        | TriviaContent.Comment (Comment.CommentOnSingleLine _)
        | TriviaContent.Newline
        | TriviaContent.Directive _ -> simpleTriviaToTriviaInstruction parentNode trivia
        | TriviaContent.Comment (Comment.LineCommentAfterSourceCode _) ->
            lineCommentAfterSourceCodeToTriviaInstruction parentNode trivia
        | TriviaContent.Comment (Comment.BlockComment _) -> blockCommentToTriviaInstruction parentNode trivia

(*
    1. Collect TriviaNodes from AST
    2. Extract trivia from directives, comments and blank lines
    3. Merge trivia with triviaNodes
    4. genTrivia should use ranges to identify what extra content should be added from what triviaNode
*)
let collectTrivia
    (config: FormatConfig)
    (source: ISourceText)
    (ast: ParsedInput)
    (selection: TriviaForSelection option)
    : TriviaInstruction list =
    let rootNode, directives, codeComments =
        match ast with
        | ParsedInput.ImplFile (ParsedImplFileInput (hds, mns, directives, codeComments)) ->
            let rootNode =
                match selection with
                | None -> astToNode ast.FullRange hds mns
                | Some { RootNode = rootNode } -> rootNode

            rootNode, directives, codeComments
        | ParsedInput.SigFile (ParsedSigFileInput (_, mns, directives, codeComments)) ->
            let rootNode =
                match selection with
                | None -> sigAstToNode ast.FullRange mns
                | Some { RootNode = rootNode } -> rootNode

            rootNode, directives, codeComments

    //printTriviaNode rootNode

    let trivia =
        let codeRange =
            match selection with
            | None -> ast.FullRange
            | Some { Selection = selection } -> selection

        [ yield! collectTriviaFromDirectives source directives selection
          yield! collectTriviaFromCodeComments source codeComments selection
          yield! collectTriviaFromBlankLines config source rootNode codeComments codeRange ]
        |> List.sortBy (fun n -> n.Range.Start.Line, n.Range.Start.Column)

    let mutable instructionCollector = ListCollector<TriviaInstruction>()

    for trivia in trivia do
        let instruction = mapTriviaToTriviaInstruction rootNode trivia

        match instruction with
        | None -> ()
        | Some instruction -> instructionCollector.Add(instruction)

    instructionCollector.Close()
