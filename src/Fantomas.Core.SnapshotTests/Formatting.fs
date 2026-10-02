/// Formatting a case, the way users get it and once per define combination, and checking what
/// came out.
module Fantomas.Core.SnapshotTests.Formatting

open System
open Fantomas.FCS.Parse
open Fantomas.FCS.Syntax
open Fantomas.FCS.Text
open Fantomas.Core
open Fantomas.Core.SyntaxOak
open Fantomas.Core.SnapshotTests.Problems

/// Trivia assignment in `Trivia.fs` takes the order of `Node.Children` to be the order in the
/// source, and every `Children` array is written by hand. Children may overlap, as ranges from the
/// parser nest, but none may start before the one in front of it. Nodes `ASTTransformer` makes up
/// carry `range0` and have no place in the source. Taken from `Fantomas.Core.Tests`.
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

/// Format the way `CodeFormatterImpl.formatDocumentWith` does, one step at a time, so that the
/// result of every define combination and the Oak it came from are in hand.
let formatEach (config: FormatConfig) (isSignature: bool) (source: string) : Formatted =
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

/// Every comment in a source, as a set of normalised texts, and how many there are: two comments
/// with the same text are one entry in the set, so the count is what notices one of them going.
let private commentsOf (isSignature: bool) (source: string) : Set<TriviaContent> * int =
    let sourceText: ISourceText = SourceText.ofString source
    let tree, _ = parseFile isSignature sourceText []

    let count: int =
        match tree with
        | ParsedInput.ImplFile(ParsedImplFileInput(trivia = trivia)) -> trivia.CodeComments.Length
        | ParsedInput.SigFile(ParsedSigFileInput(trivia = trivia)) -> trivia.CodeComments.Length

    Trivia.collectCommentTextsFromAST sourceText tree, count

let private trailingWhitespace (code: string) : int list =
    code.Replace("\r\n", "\n").Split('\n')
    |> Array.indexed
    |> Array.choose (fun (index: int, line: string) ->
        if
            line.EndsWith(" ", StringComparison.Ordinal)
            || line.EndsWith("\t", StringComparison.Ordinal)
        then
            Some(index + 1)
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

    let commentsBefore, countBefore = commentsOf isSignature source
    let commentsAfter, countAfter = commentsOf isSignature formatted.Merged

    if commentsBefore <> commentsAfter then
        problems.Add(Problem.CommentsLost(commentsBefore - commentsAfter, commentsAfter - commentsBefore))
    elif countBefore <> countAfter then
        problems.Add(Problem.CommentCountChanged(countBefore, countAfter))

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

    for output, code in (Output.Merged, formatted.Merged) :: perDefine do
        match trailingWhitespace code with
        | [] -> ()
        | lines -> problems.Add(Problem.TrailingWhitespace(output, lines))

    formatted, List.ofSeq problems
