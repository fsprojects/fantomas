module internal Fantomas.Core.Validation

open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Text
open Fantomas.FCS.Parse
open Fantomas.Core.SourceParser

let private safeToIgnoreWarnings =
    set
        [ "This construct is deprecated: it is only for use in the F# library"
          "Identifiers containing '@' are reserved for use in F# code generation" ]

// Exception of type 'FSharp.Compiler.DiagnosticsLogger+LibraryUseOnly' was thrown.

let internal noWarningOrErrorDiagnostics diagnostics =
    let errors =
        diagnostics
        |> List.filter (fun e ->
            match e.Severity with
            | FSharpDiagnosticSeverity.Error -> true
            | FSharpDiagnosticSeverity.Hidden
            | FSharpDiagnosticSeverity.Info -> false
            | FSharpDiagnosticSeverity.Warning -> not (safeToIgnoreWarnings.Contains(e.Message)))

    List.isEmpty errors

/// Check whether an input string is invalid in F# by looking for errors and warnings in the diagnostics.
let isValidFSharpCode (isSignature: bool) (source: string) : Async<bool> =
    async {
        // First get the syntax tree without any defines
        let sourceText = SourceText.ofString source
        let baseUntypedTree, baseDiagnostics = parseFile isSignature sourceText []

        let hashDirectives =
            match baseUntypedTree with
            | ImplFile(ParsedImplFileInput(_, _, directives, _))
            | SigFile(ParsedSigFileInput(_, _, directives, _)) -> directives

        match hashDirectives with
        | [] -> return noWarningOrErrorDiagnostics baseDiagnostics

        | hashDirectives ->

            let defineCombinations = Defines.getDefineCombination hashDirectives

            let isValidForCombinations =
                defineCombinations
                |> List.map (fun defineCombination ->
                    let _, diagnostics = parseFile isSignature sourceText defineCombination
                    noWarningOrErrorDiagnostics diagnostics)

            return Seq.forall id isValidForCombinations
    }
