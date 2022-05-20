module internal Fantomas.Core.Trivia

open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Fantomas.Core
open Fantomas.Core.ISourceTextExtensions
open Fantomas.Core.SourceParser
open Fantomas.Core.AstTransformer
open Fantomas.Core.TriviaTypes
open Fantomas.Core.FormatConfig

let private findFirstNodeAfterLine (nodes: TriviaNodeAssigner list) (lineNumber: int) : TriviaNodeAssigner option =
    nodes
    |> List.tryFind (fun tn -> tn.Range.StartLine > lineNumber)

let private findLastNodeOnLine (nodes: TriviaNodeAssigner list) lineNumber : TriviaNodeAssigner option =
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

let private findLastNode (nodes: TriviaNodeAssigner list) : TriviaNodeAssigner option =
    match nodes with
    | [] -> None
    | nodes ->
        nodes
        |> List.maxBy (fun tn -> tn.Range.EndLine)
        |> Some

let private findNodeBeforeLineAndColumn (nodes: TriviaNodeAssigner list) line column =
    nodes
    |> List.tryFindBack (fun tn ->
        let range = tn.Range

        range.StartLine <= line
        && range.StartColumn <= column)

let private findNodeBeforeLineFromStart (nodes: TriviaNodeAssigner list) line =
    nodes
    |> List.filter (fun tn -> tn.Range.EndLine < line)
    |> List.sortByDescending (fun tn -> tn.Range.EndLine, -tn.Range.StartColumn)
    |> List.tryFind (fun tn -> tn.Range.StartLine < line)

let private findNodeAfterLineAndColumn (nodes: TriviaNodeAssigner list) line column =
    nodes
    |> List.tryFind (fun tn ->
        let range = tn.Range

        (range.StartLine > line)
        || (range.StartLine = line
            && range.StartColumn > column))

let private commentIsAfterLastTriviaNode (triviaNodes: TriviaNodeAssigner list) (range: Range) =
    let hasNoNodesAfterRange =
        triviaNodes
        |> Seq.exists (fun tn -> tn.Range.EndLine > range.StartLine)
        |> not

    hasNoNodesAfterRange

let private updateTriviaNode (lens: TriviaNodeAssigner -> unit) (triviaNodes: TriviaNodeAssigner list) triviaNode =
    match triviaNode with
    | Some tNode ->
        // There are situations where the same range can be linked to multiple different AST nodes.
        // F.ex a getter and setter in one line.
        // We want to be sure that one node will be projected by the lens function.
        let index =
            triviaNodes
            |> List.findIndex (fun tn -> tn = tNode)

        lens triviaNodes.[index]

        triviaNodes
    | None -> triviaNodes

let private addAllTriviaAsContentAfter (trivia: Trivia list) (singleNode: TriviaNodeAssigner) =
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

let private addTriviaToTriviaNode (startOfSourceCode: int) (triviaNodes: TriviaNodeAssigner list) trivia =
    match trivia with
    | { Item = Comment (LineCommentOnSingleLine _ as comment)
        Range = range } ->
        let nodeAfterLine = findFirstNodeAfterLine triviaNodes range.StartLine

        match nodeAfterLine with
        | Some _ ->
            nodeAfterLine
            |> updateTriviaNode (fun tn -> tn.ContentBefore.Add(Comment(comment))) triviaNodes
        | None ->
            // try and find a node above
            findNodeBeforeLineFromStart triviaNodes range.StartLine
            |> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Comment(comment))) triviaNodes

    | { Item = Comment (BlockComment (comment, _, _))
        Range = range } ->
        let nodeAfter =
            findNodeAfterLineAndColumn triviaNodes range.StartLine range.StartColumn

        let nodeBefore =
            findNodeBeforeLineAndColumn triviaNodes range.StartLine range.StartColumn

        match nodeBefore, nodeAfter with
        | Some nb, Some na when
            (nb.Range.EndLine < range.StartLine
             && na.Range.StartLine > range.EndLine)
            ->
            Some na
            |> updateTriviaNode (fun tn -> tn.ContentBefore.Add(Comment(BlockComment(comment, true, true)))) triviaNodes
        | Some n, _ when n.Range.EndLine = range.StartLine ->
            Some n
            |> updateTriviaNode
                (fun tn -> tn.ContentAfter.Add(Comment(BlockComment(comment, false, false))))
                triviaNodes
        | _, Some n ->
            Some n
            |> updateTriviaNode
                (fun tn ->
                    let newline = tn.Range.StartLine > range.EndLine
                    tn.ContentBefore.Add(Comment(BlockComment(comment, false, newline))))
                triviaNodes
        | Some _, _ when (commentIsAfterLastTriviaNode triviaNodes range) ->
            findLastNode triviaNodes
            |> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Comment(BlockComment(comment, true, false)))) triviaNodes
        | Some n, _ ->
            Some n
            |> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Comment(BlockComment(comment, true, false)))) triviaNodes
        | None, None ->
            findNodeBeforeLineFromStart triviaNodes range.StartLine
            |> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Comment(BlockComment(comment, true, true)))) triviaNodes

    | { Item = Comment (LineCommentAfterSourceCode _ as comment)
        Range = range } ->
        findLastNodeOnLine triviaNodes range.EndLine
        |> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Comment(comment))) triviaNodes

    // Newlines are only relevant if they occur after the first line of source code
    | { Item = Newline; Range = range } when (range.StartLine > startOfSourceCode) ->
        let nodeAfterLine = findFirstNodeAfterLine triviaNodes range.StartLine

        match nodeAfterLine with
        | Some _ ->
            nodeAfterLine
            |> updateTriviaNode (fun tn -> tn.ContentBefore.Add(Newline)) triviaNodes
        | None ->
            // try and find a node above
            findNodeBeforeLineFromStart triviaNodes range.StartLine
            |> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Newline)) triviaNodes

    | { Item = Directive dc as directive
        Range = range } ->
        let nodeAfterLine = findFirstNodeAfterLine triviaNodes range.StartLine

        match nodeAfterLine with
        | Some _ as node -> updateTriviaNode (fun tn -> tn.ContentBefore.Add(directive)) triviaNodes node
        | None ->
            let findNode nodes =
                findNodeBeforeLineFromStart nodes range.StartLine

            findNode triviaNodes
            |> updateTriviaNode
                (fun tn ->
                    let directive = Directive dc
                    tn.ContentAfter.Add(directive))
                triviaNodes

    | _ -> triviaNodes

let private triviaNodeIsNotEmpty (triviaNode: TriviaNodeAssigner) =
    not (Seq.isEmpty triviaNode.ContentAfter)
    || not (Seq.isEmpty triviaNode.ContentBefore)
    || Option.isSome triviaNode.ContentItself

let private transformNonEmptyNodes (nodes: TriviaNodeAssigner list) : TriviaNode list =
    nodes
    |> List.choose (fun tn ->
        if triviaNodeIsNotEmpty tn then
            { Type = tn.Type
              Range = tn.Range
              ContentBefore = Seq.toList tn.ContentBefore
              ContentItself = tn.ContentItself
              ContentAfter = Seq.toList tn.ContentAfter }
            |> Some
        else
            None)

let internal collectTriviaFromDirectives
    (source: ISourceText)
    (directives: ConditionalDirectiveTrivia list)
    : Trivia list =
    directives
    |> List.map (function
        | ConditionalDirectiveTrivia.If (_, r)
        | ConditionalDirectiveTrivia.Else r
        | ConditionalDirectiveTrivia.EndIf r ->
            let text = (source.GetContentAt r).TrimEnd()
            { Item = Directive text; Range = r })

let internal collectTriviaFromCodeComments (source: ISourceText) (codeComments: CommentTrivia list) : Trivia list =
    codeComments
    |> List.map (function
        | CommentTrivia.BlockComment r ->
            { Item = Comment(BlockComment(source.GetContentAt r, false, false))
              Range = r }
        | CommentTrivia.LineComment r ->
            let content = source.GetContentAt r
            let line = source.GetLineString(r.StartLine - 1)

            let item =
                Comment(
                    if line.TrimStart(' ', ';').StartsWith("//") then
                        LineCommentOnSingleLine content
                    else
                        LineCommentAfterSourceCode content
                )

            { Item = item; Range = r })

let internal collectTriviaFromBlankLines
    (config: FormatConfig)
    (source: ISourceText)
    (triviaNodes: TriviaNodeAssigner list)
    (codeComments: CommentTrivia list)
    : Trivia list =
    let fileIndex = triviaNodes.Head.Range.FileIndex

    let captureLinesIfMultiline (r: range) =
        if r.StartLine = r.EndLine then
            []
        else
            [ r.StartLine .. r.EndLine ]

    let multilineStringsLines =
        triviaNodes
        |> List.collect (fun tn ->
            match tn.Type with
            | SynConst_String
            | SynConst_Bytes
            | SynInterpolatedStringPart_String -> captureLinesIfMultiline tn.Range
            | _ -> [])

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

    let max = source.GetLineCount() - 1

    (0, [ 0..max ])
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

(*
    1. Collect TriviaNode from tokens and AST
    2. Collect TriviaContent from tokens
    3. Merge trivias with triviaNodes
    4. genTrivia should use ranges to identify what extra content should be added from what triviaNode
*)
let collectTrivia (config: FormatConfig) (source: ISourceText) (ast: ParsedInput) : TriviaNode list =
    let triviaNodesFromAST, directives, codeComments =
        match ast with
        | ParsedInput.ImplFile (ParsedImplFileInput (hds, mns, directives, codeComments)) ->
            astToNode hds mns, directives, codeComments
        | ParsedInput.SigFile (ParsedSigFileInput (_, mns, directives, codeComments)) ->
            sigAstToNode mns, directives, codeComments

    let triviaNodes =
        triviaNodesFromAST
        |> List.sortBy (fun n -> n.Range.Start.Line, n.Range.Start.Column)

    let trivia =
        [ yield! collectTriviaFromDirectives source directives
          yield! collectTriviaFromCodeComments source codeComments
          yield! collectTriviaFromBlankLines config source triviaNodes codeComments ]
        |> List.sortBy (fun n -> n.Range.Start.Line, n.Range.Start.Column)

    let startOfSourceCode = 1
    //        match tokens with
    //        | h :: _ -> h.LineNumber // Keep track of comments or hash defines before the first AST node
    //        | _ -> 1

    match trivia with
    | [] -> []
    | _ ->
        match ast, triviaNodes with
        | EmptyFile _, h :: _ -> addAllTriviaAsContentAfter trivia h
        | _ ->
            List.fold (addTriviaToTriviaNode startOfSourceCode) triviaNodes trivia
            |> transformNonEmptyNodes
