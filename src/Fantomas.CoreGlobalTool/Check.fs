[<RequireQualifiedAccess>]
module Fantomas.Cmd.Check

open System.IO
open Fantomas.FakeHelpers
open Fantomas.FormatConfig

type ExitCode =
    | NoChanges = 0
    | NeedsFormating = 1
    | InternalError = 99

let private toCheckOutput r =
    match r with
    | FormatResult.Formatted(filename, _) -> sprintf "%s needs formatting" filename
    | FormatResult.Error(filename, exn) -> sprintf "error: Failed to format %s: %s" filename (exn.ToString())
    | FormatResult.Unchanged(_) -> failwith "No unchanged file should be present at this point"

let private reportCheckResults (output: TextWriter) (results: seq<FormatResult>) =
    results
    |> Seq.map toCheckOutput
    |> Seq.iter output.WriteLine

/// Runs a check on the given files and reports the result to the given output:
/// 
/// * It shows the paths of the files that need formatting
/// * It shows the path and the error message of files that failed the format check
/// 
/// Returns:
/// 
/// * `ExitCode.NoChanges` - if none of the files requires any changes
/// * `ExitCode.NeedsFormatting` - if at least one of the files requires formatting
/// * `ExitCode.InternalError` - if at least one of the files failed to format
let run (config: FormatConfig) (output: TextWriter) (filenames: seq<string>): ExitCode =
    async {
        let! formatted = filenames
                         |> Seq.map (formatFileAsync config)
                         |> Async.Parallel

        let isChange =
            function
            | FormatResult.Unchanged(_) -> false
            | FormatResult.Formatted(_)
            | FormatResult.Error(_) -> true

        let changes = formatted |> Seq.filter isChange

        // always print report to StdOut
        changes |> reportCheckResults output

        let isError =
            function
            | FormatResult.Error(_) -> true
            | _ -> false

        let hasErrors = changes |> Seq.exists isError

        if hasErrors then
            return ExitCode.InternalError
        else
            return match changes |> Seq.length with
                   | 0 -> ExitCode.NoChanges
                   | _ -> ExitCode.NeedsFormating
    }
    |> Async.RunSynchronously
