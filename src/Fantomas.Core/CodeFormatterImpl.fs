[<RequireQualifiedAccess>]
module internal Fantomas.Core.CodeFormatterImpl

open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open Fantomas.Core
open Fantomas.Core.FormatConfig
// open Fantomas.Core.SourceParser
// open Fantomas.Core.TriviaTypes
// open Fantomas.Core.CodePrinter
open Fantomas.Core.CodePrinter2

let getSourceText (source: string) : ISourceText = source.TrimEnd() |> SourceText.ofString

let parse (isSignature: bool) (source: ISourceText) : Async<ParsedInput> =
    // Async<(ParsedInput * DefineCombination)[]> =
    // First get the syntax tree without any defines
    let baseUntypedTree, baseDiagnostics =
        Fantomas.FCS.Parse.parseFile isSignature source []

    async { return baseUntypedTree }
// let hashDirectives =
//     match baseUntypedTree with
//     | ImplFile (ParsedImplFileInput (_, _, directives, _))
//     | SigFile (ParsedSigFileInput (_, _, directives, _)) -> directives
//
// match hashDirectives with
// | [] ->
//     async {
//         let errors =
//             baseDiagnostics
//             |> List.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
//
//         if not errors.IsEmpty then
//             raise (FormatException $"Parsing failed with errors: %A{baseDiagnostics}")
//
//         return [| (baseUntypedTree, []) |]
//     }
// | hashDirectives ->
//     let defineCombinations = Defines.getDefineCombination hashDirectives
//
//     defineCombinations
//     |> List.map (fun defineCombination ->
//         async {
//             let untypedTree, diagnostics =
//                 Fantomas.FCS.Parse.parseFile isSignature source defineCombination
//
//             let errors =
//                 diagnostics
//                 |> List.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
//
//             if not errors.IsEmpty then
//                 raise (FormatException $"Parsing failed with errors: %A{diagnostics}")
//
//             return (untypedTree, defineCombination)
//         })
//     |> Async.Parallel

/// Format an abstract syntax tree using given config
let formatAST
    (ast: ParsedInput)
    (sourceText: ISourceText option)
    (config: FormatConfig)
    // (selection: TriviaForSelection option)
    : string =
    let formattedSourceCode =
        let context = Context.Context.Create config sourceText ast //selection
        let fileNode = Fangorn.mkOak sourceText.Value ast
        context |> genFile fileNode |> Context.dump false // (Option.isSome selection)

    formattedSourceCode

let format (config: FormatConfig) (isSignature: bool) (source: ISourceText) : Async<string> =
    async {
        let! asts = parse isSignature source

        let result = formatAST asts (Some source) config
        // asts
        // |> Array.map (fun (ast', defineCombination) ->
        //     async {
        //         let formattedCode = formatAST ast' (Some source) config None
        //         return (defineCombination, formattedCode)
        //     })
        // |> Async.Parallel
        // |> Async.map Array.toList

        //         let merged =
        //             match results with
        //             | [] -> failwith "not possible"
        //             | [ (_, x) ] -> x
        //             | all ->
        //                 let allInFragments = all |> String.splitInFragments config.EndOfLine.NewLineString
        //
        //                 let allHaveSameFragmentCount =
        //                     let allWithCount = List.map (fun (_, f: string list) -> f.Length) allInFragments
        //
        //                     (Set allWithCount).Count = 1
        //
        //                 if not allHaveSameFragmentCount then
        //                     let chunkReport =
        //                         allInFragments
        //                         |> List.map (fun (defines, fragments) ->
        //                             sprintf "[%s] has %i fragments" (String.concat ", " defines) fragments.Length)
        //                         |> String.concat config.EndOfLine.NewLineString
        //
        //                     raise (
        //                         FormatException(
        //                             $"""Fantomas is trying to format the input multiple times due to the detect of multiple defines.
        // There is a problem with merging all the code back together.
        // {chunkReport}
        // Please raise an issue at https://fsprojects.github.io/fantomas-tools/#/fantomas/preview."""
        //                         )
        //                     )
        //
        //                 List.map snd allInFragments
        //                 |> List.reduce String.merge
        //                 |> String.concat config.EndOfLine.NewLineString

        // return merged
        return result
    }

/// Format a source string using given config
let formatDocument config isSignature source = format config isSignature source
