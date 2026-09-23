module internal Fantomas.Core.Trivia

open System
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Text
open Fantomas.Core.SyntaxOak

let closingDelimiters = Set.ofList [ "]"; "}"; "|}"; ")"; "|)" ]

type CommentTrivia with

    member x.Range =
        match x with
        | CommentTrivia.BlockComment m
        | CommentTrivia.LineComment m -> m

/// Groups comments that follow each other on the same line with only whitespace between them.
/// Every group holds at least one comment.
let groupCommentsOnSameLine (source: ISourceText) (comments: CommentTrivia list) : CommentTrivia list list =
    let followsOnSameLine (previous: CommentTrivia) (next: CommentTrivia) : bool =
        previous.Range.EndLine = next.Range.StartLine
        && source
            .GetLineString(next.Range.StartLine - 1)
            .Substring(previous.Range.EndColumn, next.Range.StartColumn - previous.Range.EndColumn)
           |> String.IsNullOrWhiteSpace

    let groups: ResizeArray<CommentTrivia list> = ResizeArray()
    let currentGroup: ResizeArray<CommentTrivia> = ResizeArray()

    for comment in comments do
        if
            currentGroup.Count > 0
            && not (followsOnSameLine currentGroup.[currentGroup.Count - 1] comment)
        then
            groups.Add(List.ofSeq currentGroup)
            currentGroup.Clear()

        currentGroup.Add(comment)

    if currentGroup.Count > 0 then
        groups.Add(List.ofSeq currentGroup)

    List.ofSeq groups

let internal collectTriviaFromCodeComments
    (source: ISourceText)
    (codeComments: CommentTrivia list)
    (codeRange: range)
    : TriviaNode list
    =
    let hasSourceTextBefore (r: range) : bool =
        source.GetLineString(r.StartLine - 1).Substring(0, r.StartColumn).TrimStart(' ', ';').Length > 0

    let hasSourceTextAfter (r: range) : bool =
        source.GetLineString(r.EndLine - 1).Substring(r.EndColumn).TrimEnd(' ', ';').Length > 0

    let groupToTrivia (group: CommentTrivia list) : TriviaNode list =
        assert (not (List.isEmpty group))
        let first: CommentTrivia = List.head group
        let last: CommentTrivia = List.last group

        if not (hasSourceTextBefore first.Range) && not (hasSourceTextAfter last.Range) then
            // No code on the line, the comments are a single comment on that line: `(* a *) (* b *)`.
            // See https://github.com/fsprojects/fantomas/issues/3487
            let r: range = Range.unionRanges first.Range last.Range
            [ TriviaNode(CommentOnSingleLine(source.GetSubTextFromRange r), r) ]
        else
            group
            |> List.map (fun ct ->
                match ct with
                | CommentTrivia.BlockComment r ->
                    TriviaNode(BlockComment(source.GetSubTextFromRange r, false, false), r)
                | CommentTrivia.LineComment r ->
                    TriviaNode(LineCommentAfterSourceCode(source.GetSubTextFromRange r), r)
            )

    codeComments
    |> List.filter (fun ct -> RangeHelpers.rangeContainsRange codeRange ct.Range)
    |> groupCommentsOnSameLine source
    |> List.collect groupToTrivia

let internal collectTriviaFromBlankLines
    (config: FormatConfig)
    (source: ISourceText)
    (rootNode: Node)
    (codeComments: CommentTrivia list)
    (codeRange: range)
    : TriviaNode list
    =
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
                | :? StringNode as node -> captureLinesIfMultiline node.Range
                | _ -> []

            let finalContinuation (lines: int list list) : int list =
                List.collect id (currentLines :: lines) |> finalContinuation

            Continuation.sequence continuations finalContinuation

        visit rootNode id

    let blockCommentLines =
        codeComments
        |> List.collect (
            function
            | CommentTrivia.BlockComment r -> captureLinesIfMultiline r
            | CommentTrivia.LineComment _ -> []
        )

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
            count, None
    )

type ConditionalDirectiveTrivia with

    member x.Range =
        match x with
        | ConditionalDirectiveTrivia.If(_, m)
        | ConditionalDirectiveTrivia.Else m
        | ConditionalDirectiveTrivia.Elif(_, m)
        | ConditionalDirectiveTrivia.EndIf m -> m

let internal collectTriviaFromDirectiveRanges
    (source: ISourceText)
    (directiveRanges: range list)
    (codeRange: range)
    : TriviaNode list
    =
    directiveRanges
    |> List.choose (fun directiveRange ->
        if not (RangeHelpers.rangeContainsRange codeRange directiveRange) then
            None
        else

        let text = (source.GetSubTextFromRange directiveRange).TrimEnd()
        let content = Directive text
        Some(TriviaNode(content, directiveRange))
    )

let rec findNodeWhereRangeFitsIn (root: Node) (range: range) : Node option =
    let doesSelectionFitInNode = RangeHelpers.rangeContainsRange root.Range range

    if not doesSelectionFitInNode then
        None
    else

    // The more specific the node fits the selection, the better
    let betterChildNode =
        root.Children
        |> Array.tryPick (fun childNode -> findNodeWhereRangeFitsIn childNode range)

    betterChildNode |> Option.orElseWith (fun () -> Some root)

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
    | :? ExprIfThenElseNode
    | :? ExprIfThenElifNode
    | :? ExprAppNode
    | :? ExprSameInfixAppsNode
    | :? ExprInfixAppNode
    | :? ExprLambdaNode
    | :? BindingNode
    | :? TypeDefnEnumNode
    | :? TypeDefnUnionNode
    | :? TypeDefnRecordNode
    | :? TypeNameNode
    | :? TypeDefnAbbrevNode
    | :? TypeDefnExplicitNode
    | :? TypeDefnAugmentationNode
    | :? TypeDefnDelegateNode
    | :? TypeDefnRegularNode
    | :? ExprMatchNode
    | :? PatParameterNode
    | :? PatTupleNode
    | :? TypeTupleNode
    | :? TypeAppPrefixNode
    | :? TypeAppPostFixNode
    | :? TypeFunsNode
    | :? ExprTupleNode
    | :? MemberDefnInheritNode
    | :? OpenListNode
    | :? InheritConstructorTypeOnlyNode
    | :? InheritConstructorUnitNode
    | :? InheritConstructorParenNode
    | :? InheritConstructorOtherNode
    | :? FieldNode
    | :? BindingListNode
    | :? MemberDefnExplicitCtorNode
    | :? MemberDefnInterfaceNode
    | :? MemberDefnAutoPropertyNode
    | :? MemberDefnAbstractSlotNode
    | :? MemberDefnPropertyGetSetNode
    | :? MatchClauseNode
    | :? ExprCompExprBodyNode
    | :? NestedModuleNode
    | :? UnionCaseNode
    | :? EnumCaseNode
    | :? ValNode
    | :? BindingReturnInfoNode
    | :? PatLeftMiddleRight
    | :? MultipleAttributeListNode -> visitLastChildNode (Array.last node.Children)
    | :? PatLongIdentNode
    | :? ModuleOrNamespaceNode ->
        if Array.isEmpty node.Children then
            node
        else
            visitLastChildNode (Array.last node.Children)
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
        node.AddAfter(trivia)
    )

/// Find a node that ended before the trivia and whose start column matches the trivia's column.
/// Searches top-down and returns the outermost match that is the first code on its line,
/// as its column is the indentation the comment lines up with.
/// Without such a node, the deepest node at the comment's column is returned.
///
/// Used for indented single-line comments that sit between a parent's children.
/// For example, in:
///     try ... with exn -> ()
///     // comment here
/// The comment at column 4 should attach to the try-with (which also starts at column 4),
/// not to the next top-level binding at column 0.
///
/// When a node and its last child both start at the comment's column, the node wins:
///     c +
///     d
///     // comment
/// The comment belongs to the infix expression `c + d`, not to `d`.
/// Placing it after `d` would make the expression multiline and move the operator,
/// which then no longer produces the same attachment on the next format.
///
/// A node that follows other code on its line is only picked when nothing else matches:
///     |> run (fun changes ->
///             printfn "%A" changes
///             // comment
/// The lambda starts at the comment's column, but after `|> run (`. The comment belongs to its body.
let findNodeBeforeWithMatchingColumn (node: Node) (triviaRange: range) : Node option =
    let triviaColumn = triviaRange.StartColumn
    let triviaLine = triviaRange.StartLine

    // `endLineOfPrevious` is the line where the node before `node` ends.
    // A child starts its line when the node before it ends on an earlier line.
    // Both are found by range rather than by index, so the order of `Children` does not matter.
    let rec visit (endLineOfPrevious: int) (node: Node) : Node option =
        let before: Node array =
            node.Children |> Array.filter (fun child -> child.Range.EndLine < triviaLine)

        if Array.isEmpty before then
            None
        else

        let child: Node =
            before |> Array.maxBy (fun child -> child.Range.EndLine, child.Range.EndColumn)

        let endLineBeforeChild: int =
            node.Children
            |> Array.filter (fun sibling ->
                not (obj.ReferenceEquals(sibling, child))
                && Position.posGeq child.Range.Start sibling.Range.End
            )
            |> Array.fold (fun endLine sibling -> max endLine sibling.Range.EndLine) endLineOfPrevious

        let matchesColumn: bool = child.Range.StartColumn = triviaColumn

        if matchesColumn && endLineBeforeChild < child.Range.StartLine then
            Some child
        else
            visit endLineBeforeChild child
            |> Option.orElse (if matchesColumn then Some child else None)

    // What precedes the container is unknown, so its first child is taken to start its line.
    visit 0 node

/// Assigns a trivia node (comment, blank line, directive) to the appropriate child
/// of containerNode as either ContentBefore or ContentAfter.
///
/// For indented single-line comments (column > 0), we search for a preceding node
/// at the same column. This handles cases like:
///
///     let x =
///         try foo() with _ -> ()
///         // this comment belongs to the try-with above
///     let y = 1
///
/// When both a predecessor and successor exist, the predecessor wins if:
///   - the successor is at a different column, OR
///   - the successor is a leaf node (no children, e.g. closing brackets like `|}`, `]`, `)`)
/// Leaf nodes are syntactic delimiters, not content — the comment belongs to the preceding content.
let assignTriviaToTriviaInstruction (containerNode: Node) (trivia: TriviaNode) : unit =
    let nodeAfter =
        containerNode.Children
        |> Array.tryFind (fun node -> node.Range.StartLine > trivia.Range.StartLine)

    let nodeBefore =
        match trivia.Content with
        | CommentOnSingleLine _
        | CommentOnSingleLineWithLeadingNewlines _ when trivia.Range.StartColumn > 0 ->
            findNodeBeforeWithMatchingColumn containerNode trivia.Range
        | _ -> None

    match nodeBefore, nodeAfter with
    // Predecessor at a different column than the comment — the comment is indented relative to the successor.
    // Example: try-with where the comment is at the same column as the try body, not the next top-level binding:
    //     let x =
    //         try foo() with _ -> ()
    //         // comment here (column 8, matches try-with)
    //     let y = 1              (column 4, different)
    | Some before, Some after when after.Range.StartColumn <> trivia.Range.StartColumn -> before.AddAfter(trivia)

    // Both predecessor and successor are at the same column as the comment.
    // Prefer the predecessor only when the successor is a closing delimiter (], }, |}, ), |)).
    // These are syntactic brackets, not content — the comment belongs to the preceding content.
    //
    // List/record with comment before closing bracket — predecessor wins:
    //     let list = [
    //         someItem           ← predecessor
    //         // comment
    //     ]                      ← successor: closing delimiter → comment goes to predecessor
    //
    // Same-column content siblings — successor wins (default):
    //     let a = 1              ← predecessor
    //     // comment             ← ContentBefore of next sibling
    //     let b = 2              ← successor
    //
    // Type arguments at same column — successor wins (default):
    //     System.DateTime array, ← predecessor
    //     //                     ← ContentBefore of next type arg
    //     int                    ← successor
    | Some before, Some after when after.Range.StartColumn = trivia.Range.StartColumn ->
        let isClosingDelimiter =
            match after with
            | :? SingleTextNode as stn -> Set.contains stn.Text closingDelimiters
            | _ -> false

        if isClosingDelimiter then
            before.AddAfter(trivia)
        else
            after.AddBefore(trivia)
    | Some _, Some after -> after.AddBefore(trivia)
    | Some before, None -> before.AddAfter(trivia)
    | None, Some after -> after.AddBefore(trivia)
    | None, None ->
        containerNode.Children
        |> Array.tryLast
        |> Option.iter (fun n -> n.AddAfter(trivia))

let blockCommentToTriviaInstruction (containerNode: Node) (trivia: TriviaNode) : unit =
    let nodeAfter =
        containerNode.Children
        |> Array.tryFind (fun tn ->
            let range = tn.Range

            (range.StartLine > trivia.Range.StartLine)
            || (range.StartLine = trivia.Range.StartLine
                && range.StartColumn > trivia.Range.StartColumn)
        )

    let nodeBefore =
        containerNode.Children
        |> Array.tryFindBack (fun tn ->
            let range = tn.Range

            range.EndLine <= trivia.Range.StartLine
            && range.EndColumn <= trivia.Range.StartColumn
        )
        |> Option.map visitLastChildNode

    let triviaWith newlineBefore newlineAfter =
        match trivia.Content with
        | BlockComment(content, _, _) ->
            let content = BlockComment(content, newlineBefore, newlineAfter)
            TriviaNode(content, trivia.Range)
        | _ -> trivia

    match nodeBefore, nodeAfter with
    | Some nb, None when nb.Range.EndLine = trivia.Range.StartLine -> nb.AddAfter(triviaWith false false)
    | None, Some na -> na.AddBefore(triviaWith true false)
    | Some nb, Some na ->
        if nb.Range.EndLine = trivia.Range.StartLine then
            // before (* comment *) after
            nb.AddAfter(triviaWith false false)
        elif
            (nb.Range.EndLine < trivia.Range.StartLine
             && trivia.Range.EndLine = na.Range.StartLine)
        then
            // before
            // (* comment *) after
            na.AddBefore(triviaWith false false)
    | _ -> ()

/// Pre-process the trivia sequence: when consecutive Newline trivia are followed by a
/// CommentOnSingleLine at column > 0, promote them into a single CommentOnSingleLineWithLeadingNewlines.
/// This ensures the blank lines and comment are assigned to the same node.
/// See https://github.com/fsprojects/fantomas/issues/2286
let promoteNewlinesBeforeComments (trivia: TriviaNode array) : TriviaNode array =
    let result: ResizeArray<TriviaNode> = ResizeArray(trivia.Length)
    let pendingNewlines: ResizeArray<TriviaNode> = ResizeArray()

    let flushPendingNewlines () =
        for nl in pendingNewlines do
            result.Add(nl)

        pendingNewlines.Clear()

    let lastPendingNewlineIsAdjacentTo (line: int) =
        pendingNewlines.Count > 0
        && pendingNewlines.[pendingNewlines.Count - 1].Range.StartLine + 1 = line

    for t in trivia do
        match t.Content with
        | Newline ->
            // Only accumulate if this newline is adjacent to the previous one (consecutive blank lines).
            // If there's a gap, flush the pending newlines — they belong to a different location.
            if
                pendingNewlines.Count > 0
                && not (lastPendingNewlineIsAdjacentTo t.Range.StartLine)
            then
                flushPendingNewlines ()

            pendingNewlines.Add(t)
        | CommentOnSingleLine comment when lastPendingNewlineIsAdjacentTo t.Range.StartLine && t.Range.StartColumn > 0 ->
            let startPos =
                Position.mkPos pendingNewlines.[0].Range.StartLine t.Range.StartColumn

            let combinedRange = Range.mkFileIndexRange t.Range.FileIndex startPos t.Range.End

            result.Add(
                TriviaNode(CommentOnSingleLineWithLeadingNewlines(pendingNewlines.Count, comment), combinedRange)
            )

            pendingNewlines.Clear()
        | _ ->
            flushPendingNewlines ()
            result.Add(t)

    flushPendingNewlines ()
    result.ToArray()

let addToTree (tree: Oak) (trivia: TriviaNode array) : unit =
    for trivia in trivia do
        let smallestNodeThatContainsTrivia = findNodeWhereRangeFitsIn tree trivia.Range

        match smallestNodeThatContainsTrivia with
        | None -> triviaBeforeOrAfterEntireTree tree trivia
        | Some parentNode ->

        match trivia.Content with
        | LineCommentAfterSourceCode _ -> lineCommentAfterSourceCodeToTriviaInstruction parentNode trivia
        | CommentOnSingleLine _
        | CommentOnSingleLineWithLeadingNewlines _
        | Newline
        | Directive _ -> assignTriviaToTriviaInstruction parentNode trivia
        | BlockComment _
        | Cursor -> blockCommentToTriviaInstruction parentNode trivia

let internal collectCommentTextsFromAST (sourceText: ISourceText) (ast: ParsedInput) : Set<TriviaContent> =
    let parsedTrivia =
        match ast with
        | ParsedInput.ImplFile(ParsedImplFileInput(trivia = t))
        | ParsedInput.SigFile(ParsedSigFileInput(trivia = t)) -> t

    let fullRange =
        let startPos = Position.mkPos 0 0
        let endPos = Position.mkPos sourceText.Length 0
        Range.mkRange String.Empty startPos endPos

    let normalize (content: TriviaContent) =
        match content with
        | CommentOnSingleLine s
        | LineCommentAfterSourceCode s -> CommentOnSingleLine(s.TrimEnd())
        | BlockComment(s, _, _) -> BlockComment(s.TrimEnd(), false, false)
        | other -> other

    collectTriviaFromCodeComments sourceText parsedTrivia.CodeComments fullRange
    |> List.map (fun tn -> normalize tn.Content)
    |> Set.ofList

let enrichTree (config: FormatConfig) (sourceText: ISourceText) (ast: ParsedInput) (tree: Oak) : Oak =
    let fullTreeRange = tree.Range

    let parsedTrivia =
        match ast with
        | ParsedInput.ImplFile(ParsedImplFileInput(trivia = t))
        | ParsedInput.SigFile(ParsedSigFileInput(trivia = t)) -> t

    let trivia =
        let newlines =
            collectTriviaFromBlankLines config sourceText tree parsedTrivia.CodeComments fullTreeRange

        let comments =
            collectTriviaFromCodeComments sourceText parsedTrivia.CodeComments fullTreeRange

        let directiveRanges =
            (parsedTrivia.ConditionalDirectives |> List.map _.Range)
            @ (parsedTrivia.WarnDirectives
               |> List.map (
                   function
                   | WarnDirectiveTrivia.Nowarn(m)
                   | WarnDirectiveTrivia.Warnon(m) -> m
               ))

        let directives =
            collectTriviaFromDirectiveRanges sourceText directiveRanges fullTreeRange

        [| yield! comments; yield! newlines; yield! directives |]
        |> Array.sortBy (fun n -> n.Range.Start.Line, n.Range.Start.Column)

    addToTree tree (promoteNewlinesBeforeComments trivia)
    tree

let insertCursor (tree: Oak) (cursor: pos) =
    let cursorRange = Range.mkRange (tree :> Node).Range.FileName cursor cursor
    let nodeWithCursor = findNodeWhereRangeFitsIn tree cursorRange

    match nodeWithCursor with
    | Some((:? SingleTextNode) as node) -> node.AddCursor cursor
    | _ -> addToTree tree [| TriviaNode(TriviaContent.Cursor, cursorRange) |]

    tree
