/// Formatting a case, the way users get it and once per define combination, and checking what
/// came out.
module Fantomas.Core.SnapshotTests.Formatting

open System
open System.Text.RegularExpressions
open Fantomas.FCS.Parse
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Text
open Fantomas.Core
open Fantomas.Core.SyntaxOak
open Fantomas.Core.SnapshotTests.Problems

/// Trivia assignment in `Trivia.fs` takes the order of `Node.Children` to be the order in the
/// source, and every `Children` array is written by hand. Children may overlap, as ranges from the
/// parser nest, but none may start before the one in front of it. Nodes `ASTTransformer` makes up
/// carry `range0` and have no place in the source. `Fantomas.Core.Tests` has the same check for its
/// unit tests, and the two projects share no code.
let assertChildrenInSourceOrder (oak: Oak) : unit =
    let rec visit (node: Node) : unit =
        node.Children
        |> Array.filter (fun (child: Node) -> not (Range.equals child.Range Range.range0))
        |> Array.pairwise
        |> Array.iter (fun (previous: Node, next: Node) ->
            if Position.posLt next.Range.Start previous.Range.Start then
                failwith
                    $"The children of %s{node.GetType().Name} are not in source order: %s{next.GetType().Name} %O{next.Range} comes after %s{previous.GetType().Name} %O{previous.Range}"
        )

        Array.iter visit node.Children

    visit oak

/// The result for one define combination, and the Oak it was printed from.
[<NoComparison; NoEquality>]
type ForDefines =
    {
        Defines: string list
        Code: string
        Oak: Oak
    }

/// A case formatted: the merged result users get, and the result per define combination it was
/// merged from. A source without conditional directives has one combination, the empty one.
[<NoComparison; NoEquality>]
type Formatted =
    {
        Merged: string
        Combinations: ForDefines list
    }

/// The result of every define combination on its own, before they are merged, and the Oak each
/// came from.
let formatCombinations (config: FormatConfig) (isSignature: bool) (source: string) : ForDefines list =
    let sourceText: ISourceText = CodeFormatterImpl.getSourceText source

    let trees: (ParsedInput * DefineCombination) array =
        CodeFormatterImpl.parse isSignature sourceText |> Async.RunSynchronously

    let combinations: ForDefines list =
        trees
        |> Array.toList
        |> List.map (fun (tree: ParsedInput, combination: DefineCombination) ->
            let mutable printed: Oak option = None

            let result: FormatResult =
                CodeFormatterImpl.formatASTWith
                    (fun (oak: Oak) ->
                        assertChildrenInSourceOrder oak
                        printed <- Some oak
                    )
                    tree
                    (Some sourceText)
                    config
                    None

            match printed with
            | None -> failwith "The Oak was never handed to the inspection callback."
            | Some oak ->

            {
                Defines = combination.Value
                Code = result.Code
                Oak = oak
            }
        )

    combinations

/// Format the way `CodeFormatterImpl.formatDocumentWith` does, one step at a time, so that the
/// result of every define combination and the Oak it came from are in hand.
let formatEach (config: FormatConfig) (isSignature: bool) (source: string) : Formatted =
    let combinations: ForDefines list = formatCombinations config isSignature source

    let merged: string =
        match combinations with
        | [ single ] -> single.Code
        | combinations ->

        combinations
        |> List.map (fun (each: ForDefines) -> DefineCombination each.Defines, { Code = each.Code; Cursor = None })
        |> MultipleDefineCombinations.mergeMultipleFormatResults config
        |> fun (result: FormatResult) -> result.Code

    {
        Merged = merged
        Combinations = combinations
    }

/// What users get: the whole pipeline in one call.
let formatProduction (config: FormatConfig) (isSignature: bool) (source: string) : string =
    let result: FormatResult =
        CodeFormatterImpl.formatDocumentWith ignore config isSignature (CodeFormatterImpl.getSourceText source) None
        |> Async.RunSynchronously

    result.Code

/// The trivia a source has under one define combination: its comments, as a set of normalised
/// texts and as a count, and its conditional (`#if`, `#else`, `#endif`) and warn (`#nowarn`,
/// `#warnon`) directives, as their texts in order. Two comments with the same text are one entry in
/// the set, so the count is what notices one of them going. What sits in a branch the defines leave
/// out is not trivia under them, so a source is read under each of its combinations in turn.
let private triviaOf
    (isSignature: bool)
    (source: string)
    (defines: string list)
    : Set<TriviaContent> * int * string list
    =
    let sourceText: ISourceText = SourceText.ofString source
    let tree, _ = parseFile isSignature sourceText defines

    let comments, conditional, warn =
        match tree with
        | ParsedInput.ImplFile(ParsedImplFileInput(trivia = trivia)) ->
            trivia.CodeComments, trivia.ConditionalDirectives, trivia.WarnDirectives
        | ParsedInput.SigFile(ParsedSigFileInput(trivia = trivia)) ->
            trivia.CodeComments, trivia.ConditionalDirectives, trivia.WarnDirectives

    // A directive as written, with its whitespace made single spaces.
    let textOf (range: range) : string =
        Regex.Replace(sourceText.GetSubTextFromRange range, @"\s+", " ").Trim()

    let directives: string list =
        [
            for directive in conditional do
                match directive with
                | ConditionalDirectiveTrivia.If(range = range)
                | ConditionalDirectiveTrivia.Elif(range = range)
                | ConditionalDirectiveTrivia.Else range
                | ConditionalDirectiveTrivia.EndIf range -> textOf range
            for directive in warn do
                match directive with
                | WarnDirectiveTrivia.Nowarn range
                | WarnDirectiveTrivia.Warnon range -> textOf range
        ]

    Trivia.collectCommentTextsFromAST sourceText tree, comments.Length, directives

/// What one list has that the other lacks, counting repeats.
let private without (these: string list) (those: string list) : string list =
    those
    |> List.fold
        (fun (left: string list) (taken: string) ->
            match List.tryFindIndex ((=) taken) left with
            | Some index -> List.removeAt index left
            | None -> left
        )
        these

/// The lines of a result that end inside a token or a comment spanning several lines, a triple
/// quoted string say. Whitespace at their end is content, not layout. Read from the result's own
/// Oak, under each define combination it is parsed with.
let private linesEndingInsideText
    (isSignature: bool)
    (config: FormatConfig)
    (code: string)
    (combinations: string list list)
    : Set<int>
    =
    let sourceText: ISourceText = CodeFormatterImpl.getSourceText code

    combinations
    |> List.collect (fun (defines: string list) ->
        let tree, _ = parseFile isSignature sourceText defines

        let oak: Oak =
            ASTTransformer.mkOak (Some sourceText) tree
            |> Trivia.enrichTree config sourceText tree

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
    let formatted: Formatted = formatEach config isSignature source
    let problems: ResizeArray<Problem> = ResizeArray<Problem>()
    let hasDefines: bool = formatted.Combinations.Length > 1

    // The harness takes the pipeline apart to see each combination. Production must still agree.
    let production: string = formatProduction config isSignature source

    if production <> formatted.Merged then
        problems.Add(Problem.ProductionDiffers(production, formatted.Merged))

    let validation: ValidationResult =
        Validation.validateFSharpCode isSignature formatted.Merged
        |> Async.RunSynchronously

    if not validation.IsValid then
        problems.Add(Problem.Invalid(Output.Merged, validation.Diagnostics))

    if hasDefines then
        for each in formatted.Combinations do
            let _, diagnostics =
                parseFile isSignature (SourceText.ofString each.Code) each.Defines

            match Validation.invalidatingDiagnostics diagnostics with
            | [] -> ()
            | invalid -> problems.Add(Problem.Invalid(Output.Combination each.Defines, invalid))

    // Comments and directives, compared under every define combination the source has: a comment in
    // a branch only one combination keeps is only trivia under that one.
    for each in formatted.Combinations do
        let defines: string list option = if hasDefines then Some each.Defines else None

        let commentsBefore, countBefore, directivesBefore =
            triviaOf isSignature source each.Defines

        let commentsAfter, countAfter, directivesAfter =
            triviaOf isSignature formatted.Merged each.Defines

        if commentsBefore <> commentsAfter then
            problems.Add(Problem.CommentsLost(defines, commentsBefore - commentsAfter, commentsAfter - commentsBefore))
        elif countBefore <> countAfter then
            problems.Add(Problem.CommentCountChanged(defines, countBefore, countAfter))

        // In order: merging the combinations relies on their directives coming in the same order.
        if directivesBefore <> directivesAfter then
            problems.Add(
                Problem.DirectivesChanged(
                    defines,
                    without directivesBefore directivesAfter,
                    without directivesAfter directivesBefore
                )
            )

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
        let crlf: string =
            formatProduction
                { config with
                    EndOfLine = EndOfLineStyle.CRLF
                }
                isSignature
                (source.Replace("\n", "\r\n"))

        if crlf <> expected then
            problems.Add(Problem.CrlfDiffers crlf)

    let again: string = (formatEach config isSignature formatted.Merged).Merged

    if again <> formatted.Merged then
        problems.Add(Problem.NotIdempotent(Output.Merged, again))

    if hasDefines then
        for each in formatted.Combinations do
            let sourceText: ISourceText = CodeFormatterImpl.getSourceText each.Code
            let tree, _ = parseFile isSignature sourceText each.Defines

            let again: FormatResult =
                CodeFormatterImpl.formatASTWith ignore tree (Some sourceText) config None

            if again.Code <> each.Code then
                problems.Add(Problem.NotIdempotent(Output.Combination each.Defines, again.Code))

    let perDefine: (Output * string) list =
        if not hasDefines then
            []
        else

        formatted.Combinations
        |> List.map (fun (each: ForDefines) -> Output.Combination each.Defines, each.Code)

    let allDefines: string list list =
        formatted.Combinations |> List.map (fun (each: ForDefines) -> each.Defines)

    for output, code in (Output.Merged, formatted.Merged) :: perDefine do
        let combinations: string list list =
            match output with
            | Output.Merged -> allDefines
            | Output.Combination defines -> [ defines ]

        match trailingWhitespace (linesEndingInsideText isSignature config code combinations) code with
        | [] -> ()
        | lines -> problems.Add(Problem.TrailingWhitespace(output, lines))

    formatted, List.ofSeq problems
