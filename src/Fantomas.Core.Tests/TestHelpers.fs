module Fantomas.Core.Tests.TestHelpers

open System
open Fantomas.FCS.Text
open Fantomas.Core
open Fantomas.Core.SyntaxOak
open NUnit.Framework
open FsUnit

[<assembly: Parallelizable(ParallelScope.All)>]
do ()

[<RequireQualifiedAccess>]
module String =
    let normalizeNewLine (str: string) =
        str.Replace("\r\n", "\n").Replace("\r", "\n")

let config = FormatConfig.Default
let newline = "\n"

/// Trivia assignment in `Trivia.fs` takes the order of `Node.Children` to be the order in the source,
/// and goes down into a node only when the trivia lies within the node's range.
/// Every `Children` array and most ranges are written by hand, so check both for each input the tests format:
/// no child starts before the one in front of it, and every child lies within its parent's range.
/// Children may overlap, as ranges from the parser can nest.
/// Nodes that `ASTTransformer` makes up carry `range0` and have no place in the source,
/// and neither does the module of a file without code, which carries `RangeHelpers.absoluteZeroRange`.
/// `inspect` runs it on every Oak `CodeFormatterImpl.formatDocument` printed.
let assertChildrenInPlace (oak: Oak) : unit =
    let rec visit (node: Node) : unit =
        let placed: Node array =
            node.Children
            |> Array.filter (fun child -> not (Range.equals child.Range Range.range0))

        placed
        |> Array.pairwise
        |> Array.iter (fun (previous, next) ->
            if Position.posLt next.Range.Start previous.Range.Start then
                failwith
                    $"The children of %s{node.GetType().Name} are not in source order: %s{next.GetType().Name} %O{next.Range} comes after %s{previous.GetType().Name} %O{previous.Range}"
        )

        if not (Range.equals node.Range Range.range0) then
            placed
            |> Array.iter (fun child ->
                if
                    not (RangeHelpers.isAbsoluteZero child.Range)
                    && not (RangeHelpers.rangeContainsRange node.Range child.Range)
                then
                    failwith
                        $"%s{child.GetType().Name} %O{child.Range} lies outside its parent %s{node.GetType().Name} %O{node.Range}"
            )

        Array.iter visit node.Children

    visit oak

/// Every comment and directive the parser recorded is attached to a node of the Oak, as trivia
/// before or after it. Trivia assignment that finds no node for one drops it, and the printer never
/// sees it; caught here, before printing, in the case that shows it. Matched on where each ends,
/// as folding blank lines into a comment moves where it starts.
let assertTriviaAssigned (oak: Oak) (recordedTrivia: Trivia.RecordedTrivia) : unit =
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

    let unassigned: TriviaNode list =
        recordedTrivia.Comments @ recordedTrivia.Directives
        |> List.filter (fun (trivia: TriviaNode) -> not (ends.Contains(endOf trivia)))

    match unassigned with
    | [] -> ()
    | unassigned ->

    let listed: string =
        unassigned
        |> List.map (fun (trivia: TriviaNode) -> $"%O{trivia.Range}: %O{trivia.Content}")
        |> String.concat "\n"

    failwith $"Trivia the parser recorded is attached to no node of the Oak:\n%s{listed}"

/// Both checks on every define combination `CodeFormatterImpl.formatDocument` hands over.
let inspect (tree: UnderDefines<CodeFormatterImpl.FormattedTree>) : unit =
    assertChildrenInPlace tree.Value.Oak
    assertTriviaAssigned tree.Value.Oak tree.Value.Trivia

let formatFSharpString isFsiFile (s: string) config =
    async {
        // Every check but the search, which the comparison makes redundant here: not valid F#, a
        // comment or directive not kept, or not idempotent. Stricter than what users are refused
        // output for, which is invalid F# or a comment the search misses.
        let! formatted =
            CodeFormatterImpl.formatDocument
                inspect
                config
                isFsiFile
                (CodeFormatterImpl.getSourceText s)
                None
                (Validations.Parse ||| Validations.TriviaComparison ||| Validations.Idempotency)

        let formattedCode: string = formatted.Code.Replace("\r\n", "\n")

        if not (List.isEmpty formatted.Issues) then
            failwith
                $"The formatted result failed its checks.\n%A{formatted.Issues}\nFormatted code:\n%s{formattedCode}"

        return formattedCode
    }
    |> Async.RunSynchronously

let formatSignatureString = formatFSharpString true
let formatSourceString = formatFSharpString false

let formatAST (isFsiFile: bool) (source: string) (config: FormatConfig) : string =
    async {
        let ast, _ =
            Fantomas.FCS.Parse.parseFile isFsiFile (Fantomas.FCS.Text.SourceText.ofString source) []

        // The Oak `FormatASTAsync` prints, which has no source to take trivia from.
        ASTTransformer.mkOak None ast |> assertChildrenInPlace
        let! formattedCode = CodeFormatter.FormatASTAsync(ast, config)

        let! validation = CodeFormatter.ValidateFSharpCodeAsync(isFsiFile, formattedCode)

        if not validation.IsValid then
            failwithf $"The formatted result is not valid F# code or contains warnings\n%s{formattedCode}"

        return formattedCode.Replace("\r\n", "\n")
    }
    |> Async.RunSynchronously

let isValidFSharpCode isFsiFile s =
    let validation: ValidationResult =
        CodeFormatter.ValidateFSharpCodeAsync(isFsiFile, s) |> Async.RunSynchronously

    validation.IsValid

let equal x =
    let x =
        match box x with
        | :? String as s -> s.Replace("\r\n", "\n") |> box
        | x -> x

    equal x

let inline prepend s content = s + content
let (==) actual expected = Assert.AreEqual(expected, actual)
