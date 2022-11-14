module Fantomas.Core.Flowering

open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open Fantomas.Core.FormatConfig
open Fantomas.Core.ISourceTextExtensions
open Fantomas.Core.SyntaxOak

let internal collectTriviaFromCodeComments (source: ISourceText) (codeComments: CommentTrivia list) : TriviaNode list =
    codeComments
    |> List.map (function
        | CommentTrivia.BlockComment r ->
            let content = source.GetContentAt r
            let startLine = source.GetLineString(r.StartLine - 1)
            let endLine = source.GetLineString(r.EndLine - 1)

            let content =
                if
                    startLine.TrimStart(' ', ';').StartsWith("(*")
                    && endLine.TrimEnd(' ', ';').EndsWith("*)")
                then
                    CommentOnSingleLine content
                else
                    failwith "todo 4DEC9B6F-5143-475F-A78B-542E786F3E2A"

            TriviaNode(content, r)
        | CommentTrivia.LineComment r ->
            let content = source.GetContentAt r
            let index = r.StartLine - 1
            let line = source.GetLineString index

            let content =
                let trimmedLine = line.TrimStart(' ', ';')

                if index = 0 && trimmedLine.StartsWith("#!") then // shebang
                    CommentOnSingleLine content
                else if trimmedLine.StartsWith("//") then
                    CommentOnSingleLine content
                else
                    LineCommentAfterSourceCode content

            TriviaNode(content, r))

let internal collectTriviaFromBlankLines
    (config: FormatConfig)
    (source: ISourceText)
    (rootNode: Node)
    (codeComments: CommentTrivia list)
    (codeRange: range)
    : TriviaNode list =
    if codeRange.StartLine = 0 && codeRange.EndLine = 0 then
        // weird edge cases where there is no source code but only hash defines
        []
    else
        let fileIndex = codeRange.FileIndex

        let captureLinesIfMultiline (r: range) =
            if r.StartLine = r.EndLine then
                []
            else
                [ r.StartLine .. r.EndLine ]

        let multilineStringsLines =
            let rec visit (node: Node) (finalContinuation: int list -> int list) =
                let continuations: ((int list -> int list) -> int list) list =
                    Array.toList node.Children |> List.map visit

                let currentLines =
                    match node with
                    | :? StringNode as node -> captureLinesIfMultiline (node :> Node).Range
                    | _ -> []

                let finalContinuation (lines: int list list) : int list =
                    List.collect id (currentLines :: lines) |> finalContinuation

                Continuation.sequence continuations finalContinuation

            visit rootNode id

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

        let min = System.Math.Max(0, codeRange.StartLine - 1)

        let max = System.Math.Min(source.Length - 1, codeRange.EndLine - 1)

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
                        (count + 1), Some(TriviaNode(Newline, range))
                    else
                        count, None)

let rec findNodeWhereRangeFitsIn (root: Node) (range: range) : Node option =
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

let triviaBeforeOrAfterEntireTree (rootNode: Node) (trivia: TriviaNode) : unit =
    let isBefore = trivia.Range.EndLine < rootNode.Range.StartLine

    if isBefore then
        rootNode.AddBefore(trivia)
    else
        rootNode.AddAfter(trivia)

/// Find the last child node that will be the last node of the parent node.
let rec visitLastChildNode (node: Node) : Node =
    match node with
    | :? ExprIfThenNode
    | :? ExprAppNode
    | :? ExprLambdaNode
    | :? ExprLetOrUseNode
    | :? ExprLetOrUseBangNode
    | :? ExprAndBang
    | :? BindingNode
    | :? ModuleOrNamespaceNode
    | :? TypeDefnEnumNode
    | :? TypeDefnUnionNode
    | :? TypeDefnRecordNode
    | :? TypeNameNode
    | :? TypeDefnAbbrevNode
    | :? TypeDefnExplicitNode
    | :? TypeDefnAugmentationNode
    | :? TypeDefnDelegateNode
    | :? TypeDefnRegularNode
    // | SynModuleSigDecl_Val
    // | SynModuleSigDecl_Types
    // | SynModuleSigDecl_NestedModule
    // | SynValSig_
    | :? ExprMatchNode
    // | SynMatchClause_
    | :? PatTypedNode
    | :? PatTupleNode
    | :? TypeTupleNode
    | :? TypeAppPrefixNode
    | :? TypeAppPostFixNode
    | :? TypeFunsNode
    | :? ExprTupleNode
    // | SynUnionCase_
    // | SynEnumCase_
    | :? MemberDefnInheritNode
    | :? OpenListNode
    | :? InheritConstructorTypeOnlyNode
    | :? InheritConstructorUnitNode
    | :? InheritConstructorParenNode
    | :? InheritConstructorOtherNode
    | :? FieldNode
    | :? MemberDefnExternBindingNode
    | :? BindingListNode
    | :? MemberDefnExplicitCtorNode
    | :? MemberDefnInterfaceNode
    | :? MemberDefnAutoPropertyNode
    | :? MemberDefnAbstractSlotNode
    | :? MemberDefnPropertyGetSetNode -> visitLastChildNode (Array.last node.Children)
    | :? PatLongIdentNode as pat ->
        if Seq.isEmpty pat.Children then
            node
        else
            visitLastChildNode (Seq.last node.Children)
    | _ -> node

let lineCommentAfterSourceCodeToTriviaInstruction (containerNode: Node) (trivia: TriviaNode) : unit =
    let lineNumber = trivia.Range.StartLine

    let result =
        containerNode.Children
        |> Array.filter (fun node -> node.Range.EndLine = lineNumber)
        |> Array.sortByDescending (fun node -> node.Range.StartColumn)
        |> Array.tryHead

    result
    |> Option.iter (fun node ->
        let node = visitLastChildNode node
        node.AddAfter(trivia))

let simpleTriviaToTriviaInstruction (containerNode: Node) (trivia: TriviaNode) : unit =
    containerNode.Children
    |> Array.tryFind (fun node -> node.Range.StartLine > trivia.Range.StartLine)
    |> Option.map (fun n -> n.AddBefore)
    |> Option.orElseWith (fun () -> Array.tryLast containerNode.Children |> Option.map (fun n -> n.AddAfter))
    |> Option.iter (fun f -> f trivia)

let addToTree (tree: Oak) (trivia: TriviaNode seq) =
    for trivia in trivia do
        let smallestNodeThatContainsTrivia = findNodeWhereRangeFitsIn tree trivia.Range

        match smallestNodeThatContainsTrivia with
        | None -> triviaBeforeOrAfterEntireTree tree trivia
        | Some parentNode ->
            match trivia.Content with
            | LineCommentAfterSourceCode _ -> lineCommentAfterSourceCodeToTriviaInstruction parentNode trivia
            | CommentOnSingleLine _
            | Newline -> simpleTriviaToTriviaInstruction parentNode trivia

let enrichTree (config: FormatConfig) (sourceText: ISourceText) (ast: ParsedInput) (tree: Oak) : Oak =
    let _directives, codeComments =
        match ast with
        | ParsedInput.ImplFile(ParsedImplFileInput(
            trivia = { ConditionalDirectives = directives
                       CodeComments = codeComments })) -> directives, codeComments
        | ParsedInput.SigFile(ParsedSigFileInput(
            trivia = { ConditionalDirectives = directives
                       CodeComments = codeComments })) -> directives, codeComments

    let trivia =
        let newlines =
            collectTriviaFromBlankLines config sourceText tree codeComments (tree :> Node).Range

        let comments =
            match ast with
            | ParsedInput.ImplFile(ParsedImplFileInput(trivia = trivia)) ->
                collectTriviaFromCodeComments sourceText trivia.CodeComments
            | ParsedInput.SigFile(ParsedSigFileInput(trivia = trivia)) ->
                collectTriviaFromCodeComments sourceText trivia.CodeComments

        [| yield! comments; yield! newlines |]
        |> Array.sortBy (fun n -> n.Range.Start.Line, n.Range.Start.Column)

    addToTree tree trivia
    tree
