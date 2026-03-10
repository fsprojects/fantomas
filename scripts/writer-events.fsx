#load "shared.fsx"

open System.IO
open Fantomas.Core
open Shared

let getWriterEvents (input: string) (isSignature: bool) (config: FormatConfig) (defines: string list) =
    async {
        try
            let! events = CodeFormatter.GetWriterEventsAsync(isSignature, input, config, defines)
            return events |> Array.map string |> String.concat "\n"
        with ex ->
            return $"Error while getting writer events: %A{ex}"
    }

match Array.tryHead fsi.CommandLineArgs with
| Some scriptPath ->
    let scriptFile = FileInfo(scriptPath)
    let sourceFile = FileInfo(Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__))

    if scriptFile.FullName = sourceFile.FullName then
        let sample, isSignature, config, defines = parseArgs fsi.CommandLineArgs.[1..]

        getWriterEvents sample isSignature config defines
        |> Async.RunSynchronously
        |> printfn "%s"
| _ -> printfn "Usage: dotnet fsi writer-events.fsx [--editorconfig <content>] [--define FOO,BAR] <input file>"
