#load "shared.fsx"

open System.IO
open Fantomas.Core
open Shared

let format (input: string) (isSignature: bool) (config: FormatConfig) =
    async {
        try
            let! result = CodeFormatter.FormatDocumentAsync(isSignature, input, config)
            let formattedCode = result.Code

            // Check for diagnostics in the formatted output
            let sourceText = Fantomas.FCS.Text.SourceText.ofString formattedCode
            let _, diagnostics = Fantomas.FCS.Parse.parseFile isSignature sourceText []

            for d in diagnostics do
                eprintfn "Diagnostic: %A %A %s %A" d.Severity d.ErrorNumber d.Message d.Range

            return formattedCode
        with ex ->
            return $"Error while formatting: %A{ex}"
    }

match Array.tryHead fsi.CommandLineArgs with
| Some scriptPath ->
    let scriptFile = FileInfo(scriptPath)
    let sourceFile = FileInfo(Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__))

    if scriptFile.FullName = sourceFile.FullName then
        let sample, isSignature, config = parseArgs fsi.CommandLineArgs.[1..]
        format sample isSignature config |> Async.RunSynchronously |> printfn "%s"
| _ -> printfn "Usage: dotnet fsi format.fsx [--editorconfig <content>] <input file>"
