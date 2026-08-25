module internal Fantomas.Core.Validation

open Fantomas.FCS.Diagnostics
open Fantomas.FCS.Text
open Fantomas.FCS.Syntax
open Fantomas.FCS.Parse

// See https://github.com/dotnet/fsharp/blob/2a25184293e39a635217670652b00680de04472a/src/Compiler/Driver/CompilerDiagnostics.fs#L214
// and https://github.com/dotnet/fsharp/blob/b7e747921515ae7939c7cb6885513eb80ec7ca2f/src/Compiler/FSComp.txt
// for error codes
let safeToIgnoreWarnings =
    set
        [
            35 // Deprecated
            42 // LibraryUseOnly
            46 // ReservedKeyword
            1104 // lexhlpIdentifiersContainingAtSymbolReserved
            3535 // tcUsingInterfacesWithAbstractStaticMembers
        ]

let invalidatingDiagnostics (diagnostics: FSharpParserDiagnostic list) : FSharpParserDiagnostic list =
    diagnostics
    |> List.filter (fun e ->
        match e.Severity with
        | FSharpDiagnosticSeverity.Error -> true
        | FSharpDiagnosticSeverity.Hidden
        | FSharpDiagnosticSeverity.Info -> false
        | FSharpDiagnosticSeverity.Warning ->
            match e.ErrorNumber with
            | None -> true
            | Some errorNumber -> not (safeToIgnoreWarnings.Contains(errorNumber))
    )

let noWarningOrErrorDiagnostics (diagnostics: FSharpParserDiagnostic list) : bool =
    List.isEmpty (invalidatingDiagnostics diagnostics)

let validateFSharpCode (isSignature: bool) (source: string) : Async<ValidationResult> =
    async {
        // First get the syntax tree without any defines
        let sourceText = SourceText.ofString source
        let baseUntypedTree, baseDiagnostics = parseFile isSignature sourceText []

        let hashDirectives =
            match baseUntypedTree with
            | ParsedInput.ImplFile(ParsedImplFileInput(trivia = { ConditionalDirectives = directives }))
            | ParsedInput.SigFile(ParsedSigFileInput(trivia = { ConditionalDirectives = directives })) -> directives

        match hashDirectives with
        | [] ->
            return
                {
                    Diagnostics = invalidatingDiagnostics baseDiagnostics
                }

        | hashDirectives ->

            let defineCombinations = Defines.getDefineCombination hashDirectives

            // The first combination that fails is the one reported, and the ones after it are not
            // parsed at all. Every combination is parsed from the same text, so whichever failed
            // positions a reader in the same source; and the answer to whether the whole is valid
            // was settled the moment one of them was not.
            let offending: FSharpParserDiagnostic list option =
                defineCombinations
                |> List.tryPick (fun defineCombination ->
                    let _, diagnostics = parseFile isSignature sourceText defineCombination.Value

                    match invalidatingDiagnostics diagnostics with
                    | [] -> None
                    | offending -> Some offending
                )

            return
                {
                    Diagnostics = Option.defaultValue [] offending
                }
    }
