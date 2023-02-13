[<RequireQualifiedAccess>]
module internal Fantomas.Core.CodeFormatterImpl

open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open Fantomas.Core.SyntaxOak

let getSourceText (source: string) : ISourceText = source.TrimEnd() |> SourceText.ofString

let parse (isSignature: bool) (source: ISourceText) : Async<(ParsedInput * DefineCombination)[]> =
    // First get the syntax tree without any defines
    let baseUntypedTree, baseDiagnostics =
        Fantomas.FCS.Parse.parseFile isSignature source []

    let hashDirectives =
        match baseUntypedTree with
        | ParsedInput.ImplFile(ParsedImplFileInput(trivia = { ConditionalDirectives = directives }))
        | ParsedInput.SigFile(ParsedSigFileInput(trivia = { ConditionalDirectives = directives })) -> directives

    match hashDirectives with
    | [] ->
        async {
            let errors =
                baseDiagnostics
                |> List.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

            if not errors.IsEmpty then
                raise (ParseException baseDiagnostics)

            return [| (baseUntypedTree, []) |]
        }
    | hashDirectives ->
        let defineCombinations = Defines.getDefineCombination hashDirectives

        defineCombinations
        |> List.map (fun defineCombination ->
            async {
                let untypedTree, diagnostics =
                    Fantomas.FCS.Parse.parseFile isSignature source defineCombination

                let errors =
                    diagnostics
                    |> List.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

                if not errors.IsEmpty then
                    raise (ParseException diagnostics)

                return (untypedTree, defineCombination)
            })
        |> Async.Parallel

/// Format an abstract syntax tree using given config
let formatAST
    (ast: ParsedInput)
    (sourceText: ISourceText option)
    (config: FormatConfig)
    (cursor: pos option)
    : FormatResult =
    let context = Context.Context.Create config

    let oak =
        match sourceText with
        | None -> ASTTransformer.mkOak None ast
        | Some sourceText ->
            ASTTransformer.mkOak (Some sourceText) ast
            |> Trivia.enrichTree config sourceText ast

    let oak =
        match cursor with
        | None -> oak
        | Some cursor -> Trivia.insertCursor oak cursor

    context |> CodePrinter.genFile oak |> Context.dump false

let formatDocument
    (config: FormatConfig)
    (isSignature: bool)
    (source: ISourceText)
    (cursor: pos option)
    : Async<FormatResult> =
    async {
        let! asts = parse isSignature source

        let! results =
            asts
            |> Array.map (fun (ast', defineCombination) ->
                async {
                    let formattedCode = formatAST ast' (Some source) config cursor
                    return (defineCombination, formattedCode)
                })
            |> Async.Parallel
            |> Async.map Array.toList

        let merged =
            match results with
            | [] -> failwith "not possible"
            | [ (_, x) ] -> x
            | all ->
                // TODO: we currently ignore the cursor here.
                // We would need to know which defines provided the code for each fragment.
                // If we have a cursor, we need to find the fragment that contains it and matches the defines of the cursor.

                let allInFragments =
                    all
                    |> List.map (fun (dc, { Code = code }) -> dc, code)
                    |> String.splitInFragments config.EndOfLine.NewLineString

                let allHaveSameFragmentCount =
                    let allWithCount = List.map (fun (_, f: string list) -> f.Length) allInFragments

                    (Set allWithCount).Count = 1

                if not allHaveSameFragmentCount then
                    let chunkReport =
                        allInFragments
                        |> List.map (fun (defines, fragments) ->
                            sprintf "[%s] has %i fragments" (String.concat ", " defines) fragments.Length)
                        |> String.concat config.EndOfLine.NewLineString

                    raise (
                        FormatException(
                            $"""Fantomas is trying to format the input multiple times due to the detect of multiple defines.
There is a problem with merging all the code back together.
{chunkReport}
Please raise an issue at https://fsprojects.github.io/fantomas-tools/#/fantomas/preview."""
                        )
                    )

                let mergedCode =
                    List.map snd allInFragments
                    |> List.reduce String.merge
                    |> String.concat config.EndOfLine.NewLineString

                { Code = mergedCode
                  Cursor = None
                  ProfileInfos = None }

        return merged
    }
