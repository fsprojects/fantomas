module Fantomas.Report

open System
open System.IO.Abstractions
open Serilog
open Spectre.Console
// Fantomas.Core has a FormatResult of its own. Opening Fantomas last is what makes the
// FormatResult named here the one this project defines.
open Fantomas.Core
open Fantomas
open Fantomas.Cli
open Fantomas.CommandResult
open Fantomas.Logging

// The context lines a parse failure's snippet is drawn from come from the file itself. This is
// the error path's last act before the tool gives up on the file, so reading it again is free.
let sourceOf (fs: IFileSystem) (file: string) : string =
    try
        fs.File.ReadAllText file
    with _ ->
        String.Empty

let partitionResults
    (results: #(FormatResult seq))
    : (string * ProfileInfo option) list * string list * (string * ProfileInfo option) list * (string * exn) list
    =
    (([], [], [], []), results)
    ||> Seq.fold (fun (oks, ignores, unchanged, errors) next ->
        match next with
        | FormatResult.Formatted(file, _, p) -> ((file, p) :: oks, ignores, unchanged, errors)
        | FormatResult.IgnoredFile i -> (oks, i :: ignores, unchanged, errors)
        | FormatResult.Unchanged(file, p) -> (oks, ignores, (file, p) :: unchanged, errors)
        | FormatResult.Error(file, e) -> (oks, ignores, unchanged, (file, e) :: errors)
        | FormatResult.InvalidCode(file, _) ->
            let ex: FormatException = invalidResultException file
            (oks, ignores, unchanged, (file, ex :> exn) :: errors)
    )

// A DefineParseException is a FormatException, so it has to be matched first or its own wording
// is never reached.
let describeFailure (error: exn) : string option =
    match error with
    | :? DefineParseException as dpe ->
        let combinations: string =
            dpe.Combinations
            |> List.map (fun c -> if c = "no defines" then "no defines" else $"[%s{c}]")
            |> String.concat ", "

        Some
            $"When Fantomas encounters #if directives in a file, it tries to format all possible combinations of defines and will merge all different versions back into one.\nFor %s{combinations}, however, we were not able to parse the file.\nWhile you may not use this combination in your project, Fantomas requires it to produce valid code.\nConsider fixing the code or ignoring this file.\nFor more information see: https://fsprojects.github.io/fantomas/docs/end-users/ConditionalCompilationDirectives.html"
    | :? FormatException as fe -> Some fe.Message
    | _ -> None

let reportError (env: CliEnvironment) (verbosity: VerbosityLevel) (file: string, error: exn) : unit =
    let describeOther () : string =
        let message: string =
            match verbosity with
            | VerbosityLevel.Detailed -> $"%A{error}"
            | VerbosityLevel.Normal -> describeFailure error |> Option.defaultValue String.Empty

        if String.IsNullOrEmpty message then
            $"Failed to format file: %s{file}"
        else
            $"Failed to format file: %s{file} : %s{message}"

    let source () : string = sourceOf env.FileSystem file
    let verbose: bool = verbosity = VerbosityLevel.Detailed

    // A parse failure and an invariant violation both describe themselves, positions and all,
    // rather than being reduced to a single line saying only that they happened.
    match Diagnostics.describeParseFailure file source error with
    | Some report -> env.Log.Error report
    | None ->

    match Diagnostics.describeInvariantViolation file source verbose error with
    | Some report -> env.Log.Error report
    | None -> env.Log.Error(describeOther ())

let reportProfileInfo (log: ILogger) (profile: bool) (file: string, profileInfo: ProfileInfo option) : unit =
    match profile, profileInfo with
    | true, Some pI -> log.Information $"%s{file} Line count: %d{pI.LineCount} Time taken %A{pI.TimeTaken}"
    | _ -> ()

let reportProfileInfos (console: IAnsiConsole) (profile: bool) (results: (string * ProfileInfo option) list) : unit =
    if profile && not (List.isEmpty results) then
        let table: Table = Table().AddColumns([| "File"; "Line count"; "Time taken" |])

        results
        |> List.choose (fun (f, p) -> p |> Option.map (fun p -> f, p))
        |> List.sortBy fst
        |> List.fold
            (fun (t: Table) (f, p) -> t.AddRow([| f; string<int> p.LineCount; p.TimeTaken.ToString("mm\:ss\.fff") |]))
            table
        |> console.Write

let reportFormatResults (env: CliEnvironment) (settings: CliSettings) (results: #(FormatResult seq)) : unit =
    match Seq.tryExactlyOne results with
    | Some singleResult ->
        match singleResult with
        | FormatResult.IgnoredFile f -> env.Log.Information $"%s{f} was ignored."
        | FormatResult.Error(f, e) -> reportError env settings.Verbosity (f, e)
        | FormatResult.Formatted(f, _, p) ->
            env.Log.Information $"%s{f} was formatted."
            reportProfileInfo env.Log settings.Profile (f, p)
        | FormatResult.Unchanged(f, p) ->
            env.Log.Information $"%s{f} was unchanged."
            reportProfileInfo env.Log settings.Profile (f, p)
        | FormatResult.InvalidCode(f, _) ->
            let ex: FormatException = invalidResultException f
            reportError env settings.Verbosity (f, ex)

    | None ->
        let oks, ignored, unchanged, errored = partitionResults results
        let centeredColumn (v: string) : TableColumn = TableColumn(v).Centered()

        let summary: Table =
            Table()
                .AddColumns(
                    [|
                        "[green]Formatted[/]"
                        string<int> oks.Length
                        "Ignored"
                        string<int> ignored.Length
                        "[blue]Unchanged[/]"
                        string<int> unchanged.Length
                        "[red]Errored[/]"
                        string<int> errored.Length
                    |]
                    |> Array.map centeredColumn
                )

        summary.Border <- TableBorder.MinimalDoubleHead
        env.Console.Write summary

        for file in ignored do
            env.Log.Debug $"'%s{file}' was ignored"

        for e in errored do
            reportError env settings.Verbosity e

        reportProfileInfos env.Console settings.Profile (oks @ unchanged)

let reportCheckResults (env: CliEnvironment) (checkResult: CheckResult) : unit =
    for filename, exn in checkResult.Errors do
        let source () : string = sourceOf env.FileSystem filename

        match Diagnostics.describeParseFailure filename source exn with
        | Some report -> env.Log.Error report
        | None ->

        match Diagnostics.describeInvariantViolation filename source false exn with
        | Some report -> env.Log.Error report
        | None -> env.Log.Error $"error: Failed to format %s{filename}: %s{exn.ToString()}"

    for filename in checkResult.Formatted do
        env.Log.Information $"%s{filename} needs formatting"

let describeInputProblem (problem: InputProblem) : string =
    match problem with
    | InputProblem.UnsupportedFileType path -> $"Input path '%s{path}' is an unsupported file type."
    | InputProblem.NotFound path -> $"Input path '%s{path}' not found."
    | InputProblem.NoPathGiven -> "No input path provided. Call with --help for usage information."
    | InputProblem.MultiplePathsWithOut -> "Multiple input files are not supported with the --out flag."

// What is printed and what the process ends with are decided apart from each other, so that this
// reporter and the JSON one cannot end the same run with different codes.
let reportFormatCommand (env: CliEnvironment) (settings: CliSettings) (result: FormatCommandResult) : int =
    match result with
    | FormatCommandResult.InvalidInput problem -> env.Log.Error(describeInputProblem problem)
    | FormatCommandResult.Failed error -> env.Log.Error $"%s{error.Message}"
    | FormatCommandResult.Completed results -> reportFormatResults env settings results

    result.ExitCode

let reportCheckCommand (env: CliEnvironment) (result: CheckCommandResult) : int =
    match result with
    | CheckCommandResult.InvalidInput problem -> env.Log.Error(describeInputProblem problem)
    | CheckCommandResult.Failed error -> env.Log.Error $"%s{error.Message}"
    | CheckCommandResult.Completed(ignored, checkResult) ->
        for file in ignored do
            env.Log.Debug $"'%s{file}' was ignored"

        if checkResult.IsValid then
            env.Log.Debug "No changes required."
        else
            reportCheckResults env checkResult

    result.ExitCode
