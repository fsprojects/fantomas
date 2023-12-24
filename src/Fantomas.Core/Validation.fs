module internal Fantomas.Core.Validation

open Fantomas.FCS.Diagnostics
open Fantomas.FCS.Text
open Fantomas.FCS.Syntax
open Fantomas.FCS.Parse

// See https://github.com/dotnet/fsharp/blob/2a25184293e39a635217670652b00680de04472a/src/Compiler/Driver/CompilerDiagnostics.fs#L214
// and https://github.com/dotnet/fsharp/blob/b7e747921515ae7939c7cb6885513eb80ec7ca2f/src/Compiler/FSComp.txt
// for error codes
let private safeToIgnoreWarnings =
    set
        [ 35 // Deprecated
          42 // LibraryUseOnly
          46 // ReservedKeyword
          1104 ] // lexhlpIdentifiersContainingAtSymbolReserved

let noWarningOrErrorDiagnostics (diagnostics: FSharpParserDiagnostic list) : bool =
    diagnostics
    |> List.filter (fun e ->
        match e.Severity with
        | FSharpDiagnosticSeverity.Error -> true
        | FSharpDiagnosticSeverity.Hidden
        | FSharpDiagnosticSeverity.Info -> false
        | FSharpDiagnosticSeverity.Warning ->
            match e.ErrorNumber with
            | None -> true
            | Some errorNumber -> not (safeToIgnoreWarnings.Contains(errorNumber)))
    |> List.isEmpty

/// Check whether an input string is invalid in F# by looking for errors and warnings in the diagnostics.
let isValidFSharpCode (isSignature: bool) (source: string) : Async<bool> =
    async {
        // First get the syntax tree without any defines
        let sourceText = SourceText.ofString source
        let baseUntypedTree, baseDiagnostics = parseFile isSignature sourceText []

        let hashDirectives =
            match baseUntypedTree with
            | ParsedInput.ImplFile(ParsedImplFileInput(trivia = { ConditionalDirectives = directives }))
            | ParsedInput.SigFile(ParsedSigFileInput(trivia = { ConditionalDirectives = directives })) -> directives

        match hashDirectives with
        | [] -> return noWarningOrErrorDiagnostics baseDiagnostics

        | hashDirectives ->

            let defineCombinations = Defines.getDefineCombination hashDirectives

            let isValidForCombinations =
                defineCombinations
                |> List.map (fun defineCombination ->
                    let _, diagnostics = parseFile isSignature sourceText defineCombination.Value
                    noWarningOrErrorDiagnostics diagnostics)

            return Seq.forall id isValidForCombinations
    }
