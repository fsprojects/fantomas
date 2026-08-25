module Fantomas.ProfileCommand

open System
open System.Diagnostics
open Fantomas.Core
open Fantomas.Arguments
open Fantomas.Cli
open Fantomas.CommandResult
open Fantomas.Plan

type FileTiming =
    {
        File: string
        LineCount: int
        DefineCombinations: int
        TimeTaken: TimeSpan
    }

[<NoComparison>]
type ProfileResult =
    {
        Timings: FileTiming list
        Ignored: string list
        Errors: (string * exn) list
        Elapsed: TimeSpan
    }

[<RequireQualifiedAccess; NoComparison>]
type ProfileCommandResult =
    | InvalidInput of problem: InputProblem
    | Completed of result: ProfileResult
    | Failed of error: exn

    member this.ExitCode: int =
        match this with
        | ProfileCommandResult.Completed result -> if List.isEmpty result.Errors then 0 else 1
        | ProfileCommandResult.InvalidInput _
        | ProfileCommandResult.Failed _ -> 1

// Counting line feeds rather than the platform's newline: a file written with the other platform's
// line endings has just as many lines.
let countLines (content: string) : int =
    content.Length - content.Replace("\n", "").Length

// How many times formatting this file has to parse and print it.
//
// A file with conditional directives is formatted once for every combination of defines and the
// results are merged, so a short file with an `#if` can cost more than a long one without. That is
// the answer to "why is this one slow", so the report says it rather than leaving the reader to
// find the directive themselves.
//
// The text is looked at first because the only way to ask is to parse, and a second parse of every
// file would be paid for by the many files that have no directive at all to tell the many nothing.
// `#if` inside a string or a comment costs one parse and comes back as one combination, so the
// shortcut can be wrong about the cheap case but never about the expensive one.
let defineCombinations (isSignatureFile: bool) (content: string) : int =
    if not (content.Contains "#if") then
        1
    else
        CodeFormatter.ParseAsync(isSignatureFile, content)
        |> Async.RunSynchronously
        |> Array.length

let timeOneFile (env: CliEnvironment) (file: string) : Result<FileTiming, string * exn> =
    try
        let content: string = env.FileSystem.File.ReadAllText file
        let isSignatureFile: bool = file.EndsWith(".fsi", StringComparison.Ordinal)
        let config: FormatConfig = env.ReadConfiguration file

        // Counted before the clock starts: it is what explains the time rather than part of it.
        let combinations: int = defineCombinations isSignatureFile content

        let stopwatch: Stopwatch = Stopwatch.StartNew()

        CodeFormatter.FormatDocumentAsync(isSignatureFile, content, config)
        |> Async.RunSynchronously
        |> ignore

        stopwatch.Stop()

        Ok
            {
                File = file
                LineCount = countLines content
                DefineCombinations = combinations
                TimeTaken = stopwatch.Elapsed
            }
    with error ->
        Error(file, error)

let runProfileCommand (env: CliEnvironment) (inputPath: InputPath) : ProfileCommandResult =
    try
        // A profile writes nothing, so the output path it plans against is the input itself.
        match plan env.FileSystem env.Log env.IgnoreFile inputPath OutputPath.NotKnown with
        | Error problem -> ProfileCommandResult.InvalidInput problem
        | Ok items ->

        let ignored: string list =
            items
            |> List.choose (fun item ->
                match item with
                | WorkItem.Ignored file -> Some file
                | WorkItem.Format _ -> None
            )

        let toTime: string list =
            items
            |> List.choose (fun item ->
                match item with
                | WorkItem.Ignored _ -> None
                | WorkItem.Format(inputFile, _) -> Some inputFile
            )

        // One file formatted and thrown away before anything is measured. Formatting is the first
        // thing this process does, so without it the run pays to compile the formatter on whichever
        // file happens to be first and reports that as the file being slow: a signature file of six
        // lines came back slower than one of several thousand.
        match toTime with
        | [] -> ()
        | first :: _ -> timeOneFile env first |> ignore

        let stopwatch: Stopwatch = Stopwatch.StartNew()

        // One at a time. Everything else about this command follows from that.
        let measured: Result<FileTiming, string * exn> list =
            List.map (timeOneFile env) toTime

        stopwatch.Stop()

        let timings: FileTiming list =
            measured
            |> List.choose (fun outcome ->
                match outcome with
                | Error _ -> None
                | Ok timing -> Some timing
            )
            // Slowest first: the answer is the first line, and `| head` then gives a caller the
            // short version without the command having to guess a number.
            |> List.sortByDescending (fun (timing: FileTiming) -> timing.TimeTaken)

        let errors: (string * exn) list =
            measured
            |> List.choose (fun outcome ->
                match outcome with
                | Ok _ -> None
                | Error failure -> Some failure
            )

        ProfileCommandResult.Completed
            {
                Timings = timings
                Ignored = ignored
                Errors = errors
                Elapsed = stopwatch.Elapsed
            }
    with error ->
        ProfileCommandResult.Failed error
