#load "shared.fsx"

open System.IO
open Fantomas.Core
open Fantomas.Core.SnapshotTests
open Fantomas.Core.SnapshotTests.Problems
open Shared

/// Format the way a snapshot case is formatted, and print to stderr every problem the snapshot
/// tests would fail the case on: invalid, not idempotent, a lost comment, trailing whitespace, or
/// a result that differs from what users get. Returns the result, and whether it has no problem.
let format (input: string) (isSignature: bool) (config: FormatConfig) : string * bool =
    try
        let formatted, problems = Formatting.formatAndCheck config isSignature input

        for problem in problems do
            eprintfn $"%s{describe problem}\n"

        formatted.Merged, problems.IsEmpty
    with ex ->
        eprintfn $"Error while formatting: %A{ex}"
        exit 1

/// The result for one define combination on its own, before the merge: what a case's per-define gold
/// holds. `--define no-defines` is the combination without any. When the merge of all combinations
/// fails, this shows each of them. It checks nothing: the checks need the merged result.
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

    eprintfn $"The input has no combination %s{Case.combinationName wanted}. It has: %s{names}."
    exit 1

match Array.tryHead fsi.CommandLineArgs with
| Some scriptPath ->
    let scriptFile: FileInfo = FileInfo(scriptPath)

    let sourceFile: FileInfo =
        FileInfo(Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__))

    if scriptFile.FullName = sourceFile.FullName then
        let sample, isSignature, config, defines = parseArgs fsi.CommandLineArgs.[1..]

        // Written as it is, without a newline after it, so it can be redirected over a gold.
        match defines with
        | [] ->
            let formatted, isClean = format sample isSignature config
            stdout.Write formatted
            exit (if isClean then 0 else 1)
        | defines -> stdout.Write(formatCombination sample isSignature config defines)
| _ -> printfn "Usage: dotnet fsi format.fsx [--editorconfig <content>] [--define A,B] <input file>"
