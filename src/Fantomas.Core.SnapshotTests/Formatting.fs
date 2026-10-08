/// Formatting a case, the way users get it and once per define combination, and checking what
/// came out.
module Fantomas.Core.SnapshotTests.Formatting

open System
open System.Collections.Concurrent
open Fantomas.FCS.Parse
open Fantomas.FCS.Text
open Fantomas.Core
open Fantomas.Core.SyntaxOak
open Fantomas.Core.SnapshotTests.Problems

/// Trivia assignment in `Trivia.fs` takes the order of `Node.Children` to be the order in the
/// source, and goes down into a node only when the trivia lies within the node's range. Every
/// `Children` array and most ranges are written by hand, so this checks both: no child starts
/// before the one in front of it, and every child lies within its parent's range. Children may
/// overlap, as ranges from the parser nest. Nodes `ASTTransformer` makes up carry `range0` and
/// have no place in the source, and neither does the module of a file without code, which carries
/// `RangeHelpers.absoluteZeroRange`. `Fantomas.Core.Tests` has the same check for its unit tests, and
/// the two projects share no code. Returns every child out of place, as a sentence.
let childrenOutOfPlace (oak: Oak) : string list =
    let rec visit (node: Node) : string seq =
        seq {
            let placed: Node array =
                node.Children
                |> Array.filter (fun (child: Node) -> not (Range.equals child.Range Range.range0))

            for previous, next in Array.pairwise placed do
                if Position.posLt next.Range.Start previous.Range.Start then
                    yield
                        $"The children of %s{node.GetType().Name} are not in source order: %s{next.GetType().Name} %O{next.Range} comes after %s{previous.GetType().Name} %O{previous.Range}"

            if not (Range.equals node.Range Range.range0) then
                for child in placed do
                    if
                        not (RangeHelpers.isAbsoluteZero child.Range)
                        && not (RangeHelpers.rangeContainsRange node.Range child.Range)
                    then
                        let slot: string =
                            match child with
                            | :? SingleTextNode -> snd (OakFacts.slotOf { Node = child; Parent = Some node })
                            | _ -> child.GetType().Name

                        yield
                            $"%s{node.GetType().Name}.%s{slot} %O{child.Range} lies outside %s{node.GetType().Name} %O{node.Range}"

            for child in node.Children do
                yield! visit child
        }

    visit oak |> List.ofSeq

/// Every comment and directive the parser recorded is attached to a node of the Oak, as trivia
/// before or after it. Trivia assignment that finds no node for one drops it, and the printer never
/// sees it; caught here, before printing, in the case that shows it. Matched on where each ends,
/// as folding blank lines into a comment moves where it starts. Returns each that is not, with
/// where it is.
let private unattachedTrivia (oak: Oak) (recordedTrivia: Trivia.RecordedTrivia) : string list =
    let endOf (trivia: TriviaNode) : int * int =
        trivia.Range.EndLine, trivia.Range.EndColumn

    let rec attached (node: Node) : (int * int) seq =
        seq {
            yield! Seq.map endOf node.ContentBefore
            yield! Seq.map endOf node.ContentAfter

            for child in node.Children do
                yield! attached child
        }

    let ends: Set<int * int> = attached oak |> Set.ofSeq

    recordedTrivia.Comments @ recordedTrivia.Directives
    |> List.choose (fun (trivia: TriviaNode) ->
        if ends.Contains(endOf trivia) then
            None
        else
            Some $"%O{trivia.Range}: %O{trivia.Content}"
    )

/// The result for one define combination, and the Oak it was printed from.
[<NoComparison; NoEquality>]
type FormattedCombination =
    {
        Defines: string list
        Code: string
        Oak: Oak
    }

/// A case formatted: the merged result users get, what the checks asked for found wrong with it,
/// and the result per define combination it was merged from. A source without conditional
/// directives has one combination, the empty one.
[<NoComparison; NoEquality>]
type Formatted =
    {
        Merged: string
        Issues: ValidationIssue list
        Combinations: FormattedCombination list
        /// What is wrong with the Oak of a define combination, the second pass's included.
        OakProblems: Problem list
    }

/// A case formatted the way users get it, with the checks of `validations`, and the result of every
/// define combination on its own with the Oak it was printed from. Each tree is checked for its
/// children in place and its trivia attached as `formatDocument` hands it over, the second pass's
/// too; only the first pass's are kept. What those checks find is handed back rather than raised,
/// so that a script can still show the result and the trivia of a case they fail.
let formatWith (validations: Validations) (config: FormatConfig) (isSignature: bool) (source: string) : Formatted =
    // Trees are handed over from the tasks that print them, two at a time when they finish together.
    let combinations: ConcurrentQueue<FormattedCombination> = ConcurrentQueue()
    let oakProblems: ConcurrentQueue<Problem> = ConcurrentQueue()

    let inspect (tree: UnderDefines<CodeFormatterImpl.FormattedTree>) : unit =
        let formatted: CodeFormatterImpl.FormattedTree = tree.Value
        let defines: string list = tree.Defines.Value

        match childrenOutOfPlace formatted.Oak with
        | [] -> ()
        | children -> oakProblems.Enqueue(Problem.ChildrenOutOfPlace(defines, formatted.SecondPass, children))

        match unattachedTrivia formatted.Oak formatted.Trivia with
        | [] -> ()
        | trivia -> oakProblems.Enqueue(Problem.TriviaNotAttached(defines, formatted.SecondPass, trivia))

        if not formatted.SecondPass then
            combinations.Enqueue
                {
                    Defines = defines
                    Code = formatted.Code
                    Oak = formatted.Oak
                }

    let document: FormatResult =
        CodeFormatterImpl.formatDocument
            inspect
            config
            isSignature
            (CodeFormatterImpl.getSourceText source)
            None
            validations
        |> Async.RunSynchronously

    {
        Merged = document.Code
        Issues = document.Issues
        // In an order that does not depend on which task finished first.
        Combinations =
            combinations
            |> List.ofSeq
            |> List.sortBy (fun (each: FormattedCombination) -> List.length each.Defines, each.Defines)
        OakProblems = oakProblems |> List.ofSeq |> List.sortBy describe
    }

/// A case formatted the way users get it, without checking the result.
let formatEach (config: FormatConfig) (isSignature: bool) (source: string) : Formatted =
    formatWith Validations.None config isSignature source

/// The result of every define combination on its own, before they are merged.
let formatCombinations (config: FormatConfig) (isSignature: bool) (source: string) : FormattedCombination list =
    (formatEach config isSignature source).Combinations

// The comments a search of the result did not find.
let private notFound (issues: ValidationIssue list) : string list =
    issues
    |> List.choose (fun (issue: ValidationIssue) ->
        match issue with
        | ValidationIssue.MissingComment comment -> Some comment.Text
        | _ -> None
    )

/// The lines of a result that end inside a token or a comment spanning several lines, a triple
/// quoted string say. Whitespace at their end is content, not layout. Read from the result's own
/// Oak, under each define combination it is parsed with.
let private linesEndingInsideText (isSignature: bool) (code: string) (combinations: string list list) : Set<int> =
    let sourceText: ISourceText = SourceText.ofString code

    combinations
    |> List.collect (fun (defines: string list) ->
        let tree, _ = parseFile isSignature sourceText defines
        let oak: Oak = CodeFormatter.TransformAST(tree, code)

        let tokens: range list =
            OakFacts.visits oak
            |> List.choose (fun (visit: OakFacts.Visit) ->
                match visit.Node with
                | :? SingleTextNode as token -> Some token.Range
                | _ -> None
            )

        let comments: range list =
            OakFacts.attachments oak
            |> List.map (fun (attachment: OakFacts.Attachment) -> attachment.Range)

        tokens @ comments
        |> List.filter (fun (range: range) -> range.StartLine < range.EndLine)
        |> List.collect (fun (range: range) -> [ range.StartLine .. range.EndLine - 1 ])
    )
    |> Set.ofList

let private trailingWhitespace (insideText: Set<int>) (code: string) : int list =
    code.Replace("\r\n", "\n").Split('\n')
    |> Array.indexed
    |> Array.choose (fun (index: int, line: string) ->
        let lineNumber: int = index + 1

        if
            (line.EndsWith(" ", StringComparison.Ordinal)
             || line.EndsWith("\t", StringComparison.Ordinal))
            && not (insideText.Contains lineNumber)
        then
            Some lineNumber
        else
            None
    )
    |> Array.toList

/// Format a case and check everything that holds for every case. Returns what it formatted, and
/// the problems it found.
let formatAndCheck (config: FormatConfig) (isSignature: bool) (source: string) : Formatted * Problem list =
    // Every check on the result.
    let formatted: Formatted = formatWith Validations.All config isSignature source
    let problems: ResizeArray<Problem> = ResizeArray<Problem>(formatted.OakProblems)
    let hasDefines: bool = formatted.Combinations.Length > 1

    // The combination is only worth naming when the source has `#if`.
    let named (defines: string list) : string list option =
        if hasDefines then Some defines else None

    // The search a format run does for every comment, which refuses the output when it misses one.
    // The comparison is stricter, so a comment the search misses on a case that passes it is a file
    // users could not format.
    match notFound formatted.Issues with
    | [] -> ()
    | missing -> problems.Add(Problem.CommentsNotFound missing)

    for issue in formatted.Issues do
        match issue with
        | ValidationIssue.MissingComment _ -> ()
        | ValidationIssue.NotValidFSharp(_, diagnostics) -> problems.Add(Problem.Invalid(Output.Merged, diagnostics))
        | ValidationIssue.CommentsChanged(defines, missing, added) ->
            problems.Add(
                Problem.CommentsChanged(
                    named defines,
                    List.map (fun (comment: SourceComment) -> comment.Text) missing,
                    added
                )
            )
        | ValidationIssue.DirectivesChanged(defines, missing, added) ->
            problems.Add(Problem.DirectivesChanged(named defines, missing, added))
        | ValidationIssue.NotIdempotent again -> problems.Add(Problem.NotIdempotent(Output.Merged, again))
        | ValidationIssue.CheckFailed(check, error) -> problems.Add(Problem.CheckFailed(check, error))

    if hasDefines then
        for each in formatted.Combinations do
            let _, diagnostics =
                parseFile isSignature (SourceText.ofString each.Code) each.Defines

            match Validation.invalidatingDiagnostics diagnostics with
            | [] -> ()
            | invalid -> problems.Add(Problem.Invalid(Output.Combination each.Defines, invalid))

    // Windows writes `\r\n`. A case is read and formatted with `\n`, so it is formatted once more with
    // `\r\n` in and out, and must give the same result with `\r\n` line endings. A case at
    // `end_of_line = crlf` already gives `\r\n`, so its result is what has to come back.
    let expectedCrlf: string option =
        match config.EndOfLine with
        | EndOfLineStyle.LF -> Some(formatted.Merged.Replace("\n", "\r\n"))
        | EndOfLineStyle.CRLF -> Some formatted.Merged
        | EndOfLineStyle.CR -> None

    match expectedCrlf with
    | None -> ()
    | Some expected ->
        let crlf: Formatted =
            formatWith
                Validations.CommentSearch
                { config with
                    EndOfLine = EndOfLineStyle.CRLF
                }
                isSignature
                (source.Replace("\n", "\r\n"))

        if crlf.Merged <> expected then
            problems.Add(Problem.CrlfDiffers crlf.Merged)

        // A comment spanning lines carries the line endings of the source, and the result those of
        // the configuration.
        match notFound crlf.Issues with
        | [] -> ()
        | missing -> problems.Add(Problem.CommentsNotFound missing)

    if hasDefines then
        for each in formatted.Combinations do
            let tree, _ =
                parseFile isSignature (CodeFormatterImpl.getSourceText each.Code) each.Defines

            let again: string =
                CodeFormatter.FormatASTAsync(tree, config, each.Code) |> Async.RunSynchronously

            if again <> each.Code then
                problems.Add(Problem.NotIdempotent(Output.Combination each.Defines, again))

    let perDefine: (Output * string) list =
        if not hasDefines then
            []
        else

        formatted.Combinations
        |> List.map (fun (each: FormattedCombination) -> Output.Combination each.Defines, each.Code)

    let allDefines: string list list =
        formatted.Combinations
        |> List.map (fun (each: FormattedCombination) -> each.Defines)

    for output, code in (Output.Merged, formatted.Merged) :: perDefine do
        let combinations: string list list =
            match output with
            | Output.Merged -> allDefines
            | Output.Combination defines -> [ defines ]

        match trailingWhitespace (linesEndingInsideText isSignature code combinations) code with
        | [] -> ()
        | lines -> problems.Add(Problem.TrailingWhitespace(output, lines))

    formatted, List.ofSeq problems
