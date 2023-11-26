module internal Fantomas.Core.Validation

open Fantomas.FCS.Diagnostics
open Fantomas.FCS.Text
open Fantomas.FCS.Syntax
open Fantomas.FCS.Parse

let safeToIgnoreWarnings =
    function
    | "This construct is deprecated: it is only for use in the F# library"
    | "Identifiers containing '@' are reserved for use in F# code generation"
    | String.IsMatch "The identifier '\w+' is reserved for future use by F#" -> true
    | _ -> false

// Exception of type 'Fantomas.FCS.DiagnosticsLogger+LibraryUseOnly' was thrown.

let noWarningOrErrorDiagnostics diagnostics =
    diagnostics
    |> List.filter (fun e ->
        match e.Severity with
        | FSharpDiagnosticSeverity.Error -> true
        | FSharpDiagnosticSeverity.Hidden
        | FSharpDiagnosticSeverity.Info -> false
        | FSharpDiagnosticSeverity.Warning -> not (safeToIgnoreWarnings e.Message))
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
