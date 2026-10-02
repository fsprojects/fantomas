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

/// The result for one define combination on its own, before the merge: what a case's per-define gold
/// holds. `--define no-defines` is the combination without any. When the merge of all combinations
/// fails, this shows each of them.
let formatCombination (input: string) (isSignature: bool) (config: FormatConfig) (defines: string list) : string =
    let wanted: string list =
        defines
        |> List.filter (fun (define: string) -> define <> "" && define <> "no-defines")
        |> List.sort

    let combinations: Formatting.ForDefines list =
        Formatting.formatCombinations config isSignature input

    match
        combinations
        |> List.tryFind (fun (each: Formatting.ForDefines) -> List.sort each.Defines = wanted)
    with
    | Some each -> each.Code
    | None ->

    let names: string =
        combinations
        |> List.map (fun (each: Formatting.ForDefines) -> Case.combinationName each.Defines)
        |> String.concat ", "

    $"The input has no combination %s{Case.combinationName wanted}. It has: %s{names}."

match Array.tryHead fsi.CommandLineArgs with
| Some scriptPath ->
    let scriptFile: FileInfo = FileInfo(scriptPath)

    let sourceFile: FileInfo =
        FileInfo(Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__))

    if scriptFile.FullName = sourceFile.FullName then
        let sample, isSignature, config, defines = parseArgs fsi.CommandLineArgs.[1..]

        match defines with
        | [] -> format sample isSignature config |> printfn "%s"
        | defines -> formatCombination sample isSignature config defines |> printfn "%s"
| _ -> printfn "Usage: dotnet fsi format.fsx [--editorconfig <content>] [--define A,B] <input file>"
