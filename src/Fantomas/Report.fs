module Fantomas.Report

open System
open System.IO
open Spectre.Console
// Fantomas.Core has a FormatResult of its own. Opening Fantomas last is what makes the
// FormatResult named here the one this project defines.
open Fantomas.Core
open Fantomas
open Fantomas.Logging
open Fantomas.CommandResult

// The context lines a parse failure's snippet is drawn from come from the file itself. This is
// the error path's last act before the tool gives up on the file, so reading it again is free.
let sourceOf (file: string) : string =
    try
        File.ReadAllText file
    with _ ->
        String.Empty

let partitionResults
    (results: #(FormatResult seq))
    : (string * ProfileInfo option) list * string list * (string * ProfileInfo option) list * (string * Exception) list =
    (([], [], [], []), results)
    ||> Seq.fold (fun (oks, ignores, unchanged, errors) next ->
        match next with
        | FormatResult.Formatted(file, _, p) -> ((file, p) :: oks, ignores, unchanged, errors)
        | FormatResult.IgnoredFile i -> (oks, i :: ignores, unchanged, errors)
        | FormatResult.Unchanged(file, p) -> (oks, ignores, (file, p) :: unchanged, errors)
        | FormatResult.Error(file, e) -> (oks, ignores, unchanged, (file, e) :: errors)
        | FormatResult.InvalidCode(file, _) ->
            let ex = Format.invalidResultException file
            (oks, ignores, unchanged, (file, ex :> Exception) :: errors))

let reportError (verbosity: VerbosityLevel) (file: string, exn: Exception) : unit =
    let describeOther () : string =
        let message =
            match verbosity with
            | VerbosityLevel.Normal ->
                match exn with
                | :? DefineParseException as dpe ->
                    let combinations =
                        dpe.Combinations
                        |> List.map (fun c -> if c = "no defines" then "no defines" else $"[%s{c}]")
                        |> String.concat ", "

                    $"When Fantomas encounters #if directives in a file, it tries to format all possible combinations of defines and will merge all different versions back into one.\nFor %s{combinations}, however, we were not able to parse the file.\nWhile you may not use this combination in your project, Fantomas requires it to produce valid code.\nConsider fixing the code or ignoring this file.\nFor more information see: https://fsprojects.github.io/fantomas/docs/end-users/ConditionalCompilationDirectives.html"
                | :? FormatException as fe -> fe.Message
                | _ -> ""
            | VerbosityLevel.Detailed -> $"%A{exn}"

        if String.IsNullOrEmpty message then
            $"Failed to format file: %s{file}"
        else
            $"Failed to format file: %s{file} : %s{message}"

    // A parse failure describes itself, positions and all, rather than being reduced to a
    // single line saying only that it happened.
    match Diagnostics.describeParseFailure file (sourceOf file) exn with
    | Some parseFailure -> elog parseFailure
    | None -> elog (describeOther ())

let reportProfileInfo (profile: bool) (file: string, profileInfo: ProfileInfo option) : unit =
    match profile, profileInfo with
    | true, Some pI -> stdlog $"%s{file} Line count: %d{pI.LineCount} Time taken %A{pI.TimeTaken}"
    | _ -> ()

let reportProfileInfos (profile: bool) (results: (string * ProfileInfo option) list) : unit =
    if profile && not (List.isEmpty results) then
        let table = Table().AddColumns([| "File"; "Line count"; "Time taken" |])

        results
        |> List.choose (fun (f, p) -> p |> Option.map (fun p -> f, p))
        |> List.sortBy fst
        |> List.fold
            (fun (t: Table) (f, p) -> t.AddRow([| f; string<int> p.LineCount; p.TimeTaken.ToString("mm\:ss\.fff") |]))
            table
        |> AnsiConsole.Write

let reportFormatResults (profile: bool) (verbosity: VerbosityLevel) (results: #(FormatResult seq)) : int =
    match Seq.tryExactlyOne results with
    | Some singleResult ->
        match singleResult with
        | FormatResult.Formatted(f, _, p) ->
            stdlog $"%s{f} was formatted."
            reportProfileInfo profile (f, p)
            0
        | FormatResult.IgnoredFile f ->
            stdlog $"%s{f} was ignored."
            0
        | FormatResult.Unchanged(f, p) ->
            stdlog $"%s{f} was unchanged."
            reportProfileInfo profile (f, p)
            0
        | FormatResult.Error(f, e) ->
            reportError verbosity (f, e)
            1
        | FormatResult.InvalidCode(f, _) ->
            let ex = Format.invalidResultException f
            reportError verbosity (f, ex)
            1

    | None ->
        let oks, ignored, unchanged, errored = partitionResults results
        let centeredColumn (v: string) = TableColumn(v).Centered()

        let summary =
            Table()
                .AddColumns(
                    [| "[green]Formatted[/]"
                       string<int> oks.Length
                       "Ignored"
                       string<int> ignored.Length
                       "[blue]Unchanged[/]"
                       string<int> unchanged.Length
                       "[red]Errored[/]"
                       string<int> errored.Length |]
                    |> Array.map centeredColumn
                )

        summary.Border <- TableBorder.MinimalDoubleHead
        AnsiConsole.Write summary

        for e in errored do
            reportError verbosity e

        reportProfileInfos profile (oks @ unchanged)

        if errored.Length > 0 then 1 else 0

let reportCheckResults (checkResult: CheckResult) =
    for filename, exn in checkResult.Errors do
        match Diagnostics.describeParseFailure filename (sourceOf filename) exn with
        | Some parseFailure -> elog parseFailure
        | None -> elog $"error: Failed to format %s{filename}: %s{exn.ToString()}"

    for filename in checkResult.Formatted do
        stdlog $"%s{filename} needs formatting"

let describeInputProblem (problem: InputProblem) : string =
    match problem with
    | InputProblem.UnsupportedFileType path -> $"Input path '%s{path}' is an unsupported file type."
    | InputProblem.NotFound path -> $"Input path '%s{path}' not found."
    | InputProblem.NoPathGiven -> "No input path provided. Call with --help for usage information."
    | InputProblem.MultiplePathsWithOut -> "Multiple input files are not supported with the --out flag."

let reportFormatCommand (profile: bool) (verbosity: VerbosityLevel) (result: FormatCommandResult) : int =
    match result with
    | FormatCommandResult.InvalidInput problem ->
        elog (describeInputProblem problem)
        1
    | FormatCommandResult.IgnoredFile file ->
        logGrEqDetailed $"'%s{file}' was ignored"
        0
    | FormatCommandResult.Failed error ->
        elog $"%s{error.Message}"
        1
    | FormatCommandResult.Completed results -> reportFormatResults profile verbosity results

let reportCheckCommand (result: CheckCommandResult) : int =
    match result with
    | CheckCommandResult.InvalidInput problem ->
        elog (describeInputProblem problem)
        1
    | CheckCommandResult.IgnoredFile file ->
        logGrEqDetailed $"'%s{file}' was ignored"
        0
    | CheckCommandResult.Completed checkResult ->
        if checkResult.IsValid then
            logGrEqDetailed "No changes required."
            0
        else
            reportCheckResults checkResult
            if checkResult.HasErrors then 1 else 99
