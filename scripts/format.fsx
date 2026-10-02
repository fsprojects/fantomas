#load "shared.fsx"

open System.IO
open Fantomas.Core
open Fantomas.Core.SnapshotTests
open Fantomas.Core.SnapshotTests.Problems
open Shared

/// Format the way a snapshot case is formatted, and print to stderr every problem the snapshot
/// tests would fail the case on: invalid, not idempotent, a lost comment, trailing whitespace, or
/// a result that differs from what users get.
let format (input: string) (isSignature: bool) (config: FormatConfig) : string =
    try
        let formatted, problems = Formatting.formatAndCheck config isSignature input

        for problem in problems do
            eprintfn $"%s{describe problem}\n"

        formatted.Merged
    with ex ->
        $"Error while formatting: %A{ex}"

match Array.tryHead fsi.CommandLineArgs with
| Some scriptPath ->
    let scriptFile: FileInfo = FileInfo(scriptPath)

    let sourceFile: FileInfo =
        FileInfo(Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__))

    if scriptFile.FullName = sourceFile.FullName then
        let sample, isSignature, config, _ = parseArgs fsi.CommandLineArgs.[1..]
        format sample isSignature config |> printfn "%s"
| _ -> printfn "Usage: dotnet fsi format.fsx [--editorconfig <content>] <input file>"
