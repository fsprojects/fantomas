module internal Fantomas.Core.Trivia

open System
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Text
open Fantomas.Core.SyntaxOak

let openingDelimiters: Set<string> = Set.ofList [ "["; "[|"; "{"; "{|"; "(" ]
let closingDelimiters: Set<string> = Set.ofList [ "]"; "}"; "|}"; ")"; "|)" ]

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

    // Every node of the tree is visited to find a handful of strings, so the walk keeps its own
    // stack rather than building a continuation and a list for each node it passes.
    let multilineStringsLines =
        let lines: ResizeArray<int> = ResizeArray()

        let pending: System.Collections.Generic.Stack<Node> =
            System.Collections.Generic.Stack()

        pending.Push rootNode

        while pending.Count > 0 do
            let node: Node = pending.Pop()

            match node with
            | :? StringNode as node -> lines.AddRange(captureLinesIfMultiline node.Range)
            | _ -> ()

            for child in node.Children do
                pending.Push child

        lines

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

    // The child ending on the comment's line that starts furthest right, the first of them on a
    // tie. A loop rather than filtering and sorting the children for every such comment.
    let result =
        let mutable best: Node option = None

        for node in containerNode.Children do
            if node.Range.EndLine = lineNumber then
                match best with
                | Some b when b.Range.StartColumn >= node.Range.StartColumn -> ()
                | _ -> best <- Some node

        best

    result
    |> Option.iter (fun node ->
        let node = visitLastChildNode node
        node.AddAfter(trivia)
    )

/// Find a node to add a line comment to as trivia after.
///
/// The candidates are the nodes the comment directly follows: the child of the container that ends
/// last above the comment, then that child's child that ends last above it, and so on down. In
///     c +
///     d
///     // comment
/// they are `c + d` and then `d`. The comment's column picks one of them.
let findNodeBeforeWithMatchingColumn (sourceText: ISourceText) (container: Node) (triviaRange: range) : Node option =
    let triviaColumn: int = triviaRange.StartColumn
    let triviaLine: int = triviaRange.StartLine

    let endsAboveComment (node: Node) : bool = node.Range.EndLine < triviaLine
    let startsOnCommentColumn (node: Node) : bool = node.Range.StartColumn = triviaColumn

    // Only whitespace comes before the node on its line.
    let startsItsLine (node: Node) : bool =
        let line: string = sourceText.GetLineString(node.Range.StartLine - 1)
        MemoryExtensions.IsWhiteSpace(line.AsSpan(0, min node.Range.StartColumn line.Length))

    // Outermost first. `Children` are in source order, so searching them from the end finds the
    // child that ends last above the comment.
    let candidates: Node list =
        container
        |> List.unfold (fun (parent: Node) ->
            Array.tryFindBack endsAboveComment parent.Children
            |> Option.map (fun (child: Node) -> child, child)
        )

    let candidatesOnColumn: Node list = List.filter startsOnCommentColumn candidates

    // A comment on the column of what opens the container lines up with the container, not its
    // content: its first child (`[`, `<@`, `begin`, `task`, `foo` of `foo<...>`), and an opening
    // bracket after it (`{|` of `struct {|`).
    let isContentOnCommentColumn (child: Node) : bool =
        match child with
        | :? SingleTextNode as stn when Set.contains stn.Text openingDelimiters -> false
        | _ -> startsOnCommentColumn child

    // From the second child on, while they end above the comment.
    let linesUpWithEarlierContent: bool =
        let children: Node array = container.Children

        let rec linesUpFrom (index: int) : bool =
            index < children.Length
            && endsAboveComment children.[index]
            && (isContentOnCommentColumn children.[index] || linesUpFrom (index + 1))

        linesUpFrom 1

    match List.tryFind startsItsLine candidatesOnColumn, List.tryLast candidatesOnColumn with
    | Some outermost, _ ->
        // c +
        // d
        // // comment    after `c + d`, not `d`: after `d` the infix turns multiline and
        //               the comment lands elsewhere on the next format
        Some outermost
    | None, Some innermost ->
        // let a =     b
        //             // comment    after `b`, on the column after other code on its line
        Some innermost
    | None, None when linesUpWithEarlierContent ->
        // struct (int * string * bool
        //         // comment    after `bool`, the first candidate: no candidate is on the column,
        //         )             but `int`, an earlier child of the container, is
        List.tryHead candidates
    | None, None -> None

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
let assignTriviaToTriviaInstruction (sourceText: ISourceText) (containerNode: Node) (trivia: TriviaNode) : unit =
    let nodeAfter =
        containerNode.Children
        |> Array.tryFind (fun node -> node.Range.StartLine > trivia.Range.StartLine)

    let nodeBefore =
        match trivia.Content with
        | CommentOnSingleLine _
        | CommentOnSingleLineWithLeadingNewlines _ when trivia.Range.StartColumn > 0 ->
            findNodeBeforeWithMatchingColumn sourceText containerNode trivia.Range
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

let addToTree (sourceText: ISourceText) (tree: Oak) (trivia: TriviaNode array) : unit =
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
        | Directive _ -> assignTriviaToTriviaInstruction sourceText parentNode trivia
        | BlockComment _
        | Cursor -> blockCommentToTriviaInstruction parentNode trivia

[<NoComparison; NoEquality>]
type RecordedTrivia =
    {
        Comments: TriviaNode list
        Directives: TriviaNode list
    }

// Only what lies within `treeRange` is read out of the source. The tree of a whole file covers every
// comment and directive of it; a selection covers a few.
let collectRecordedTrivia (sourceText: ISourceText) (ast: ParsedInput) (treeRange: range) : RecordedTrivia =
    let parsedTrivia =
        match ast with
        | ParsedInput.ImplFile(ParsedImplFileInput(trivia = t))
        | ParsedInput.SigFile(ParsedSigFileInput(trivia = t)) -> t

    let directiveRanges: range list =
        (parsedTrivia.ConditionalDirectives |> List.map _.Range)
        @ (parsedTrivia.WarnDirectives
           |> List.map (
               function
               | WarnDirectiveTrivia.Nowarn(m)
               | WarnDirectiveTrivia.Warnon(m) -> m
           ))

    {
        Comments = collectTriviaFromCodeComments sourceText parsedTrivia.CodeComments treeRange
        Directives = collectTriviaFromDirectiveRanges sourceText directiveRanges treeRange
    }

let enrichTree (config: FormatConfig) (sourceText: ISourceText) (ast: ParsedInput) (tree: Oak) : Oak * RecordedTrivia =
    let fullTreeRange = tree.Range

    // What the parser recorded, read before any of it is assigned, and handed back with the tree:
    // what was assigned is what the formatted result is checked against.
    let recordedTrivia: RecordedTrivia =
        collectRecordedTrivia sourceText ast fullTreeRange

    let parsedTrivia =
        match ast with
        | ParsedInput.ImplFile(ParsedImplFileInput(trivia = t))
        | ParsedInput.SigFile(ParsedSigFileInput(trivia = t)) -> t

    let trivia =
        let newlines =
            collectTriviaFromBlankLines config sourceText tree parsedTrivia.CodeComments fullTreeRange

        [|
            yield! recordedTrivia.Comments
            yield! newlines
            yield! recordedTrivia.Directives
        |]
        |> Array.sortBy (fun n -> n.Range.Start.Line, n.Range.Start.Column)

    addToTree sourceText tree (promoteNewlinesBeforeComments trivia)
    tree, recordedTrivia

let insertCursor (tree: Oak) (cursor: pos) =
    let cursorRange = Range.mkRange (tree :> Node).Range.FileName cursor cursor
    let cursorTrivia: TriviaNode = TriviaNode(TriviaContent.Cursor, cursorRange)

    match findNodeWhereRangeFitsIn tree cursorRange with
    | Some(:? SingleTextNode as node) -> node.AddCursor cursor
    | Some parentNode -> blockCommentToTriviaInstruction parentNode cursorTrivia
    | None -> triviaBeforeOrAfterEntireTree tree cursorTrivia

    tree
