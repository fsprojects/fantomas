module Fantomas.JsonReport

open System.IO
open System.Text
open System.Text.Encodings.Web
open System.Text.Json
open Fantomas.Core
open Fantomas.FCS.Parse
open Fantomas.CommandResult
open Fantomas.Report

[<Literal>]
let SchemaVersion: int = 1

type Range =
    {
        StartLine: int
        StartColumn: int
        EndLine: int
        EndColumn: int
    }

type Diagnostic =
    {
        Severity: string
        Code: string
        Message: string
        Range: Range option
    }

[<RequireQualifiedAccess; NoComparison>]
type FileOutcome =
    | Formatted
    | Unchanged
    | Ignored
    | NeedsFormatting
    | Failed of message: string * diagnostics: Diagnostic list

[<NoComparison>]
type FileReport = { Path: string; Outcome: FileOutcome }

[<RequireQualifiedAccess; Struct>]
type Command =
    | Format
    | Check

[<NoComparison>]
type RunReport =
    {
        Command: Command
        WorkingDirectory: string
        ExitCode: int
        Error: string option
        Files: FileReport list
    }

// The parser counts columns from zero and the compiler prints them from one. The text report
// already resolved that in favour of the compiler, so this does the same rather than handing a
// reader two conventions to keep straight.
let describeRange (range: Fantomas.FCS.Text.range) : Range =
    {
        StartLine = range.StartLine
        StartColumn = range.StartColumn + 1
        EndLine = range.EndLine
        EndColumn = range.EndColumn + 1
    }

// The message keeps its newlines here, where the one line text form has to flatten them.
let describeDiagnostic (diagnostic: FSharpParserDiagnostic) : Diagnostic =
    {
        Severity = Diagnostics.severityText diagnostic
        Code = Diagnostics.errorNumber diagnostic
        Message = diagnostic.Message
        Range = Option.map describeRange diagnostic.Range
    }

// A parse failure is the one error worth taking apart: its positions are what a caller can act on
// without opening the file. Everything else has only a message, so it is carried as one.
let describeFileFailure (file: string) (error: exn) : FileOutcome =
    match error with
    | :? ParseException as parseFailure ->
        FileOutcome.Failed(
            $"%s{file} could not be parsed by Fantomas",
            List.map describeDiagnostic parseFailure.Diagnostics
        )
    | _ ->
        let message: string = describeFailure error |> Option.defaultValue error.Message

        FileOutcome.Failed(message, [])

// The order the files were worked on is the order a folder walk happened to return them in, which
// is neither the order they were given nor one worth promising. Sorting is a rule that can be
// stated, and it holds for both commands, where the check report has three lists to merge anyway.
let sortByPath (files: FileReport list) : FileReport list =
    List.sortBy (fun (file: FileReport) -> file.Path) files

let describeResult (result: FormatResult) : FileReport =
    match result with
    | FormatResult.IgnoredFile file ->
        {
            Path = file
            Outcome = FileOutcome.Ignored
        }
    | FormatResult.Unchanged file ->
        {
            Path = file
            Outcome = FileOutcome.Unchanged
        }
    | FormatResult.Formatted(file, _) ->
        {
            Path = file
            Outcome = FileOutcome.Formatted
        }
    | FormatResult.Error(file, error) ->
        {
            Path = file
            Outcome = describeFileFailure file error
        }
    | FormatResult.InvalidCode(file, _) ->
        {
            Path = file
            Outcome = describeFileFailure file (invalidResultException file)
        }

let formatReport (workingDirectory: string) (result: FormatCommandResult) : RunReport =
    let error, files: string option * FileReport list =
        match result with
        | FormatCommandResult.Failed error -> Some error.Message, []
        | FormatCommandResult.InvalidInput problem -> Some(describeInputProblem problem), []
        | FormatCommandResult.Completed results ->
            None, results |> Array.map describeResult |> List.ofArray |> sortByPath

    {
        Command = Command.Format
        WorkingDirectory = workingDirectory
        ExitCode = result.ExitCode
        Error = error
        Files = files
    }

let checkReport (workingDirectory: string) (result: CheckCommandResult) : RunReport =
    let error, files: string option * FileReport list =
        match result with
        | CheckCommandResult.Failed error -> Some error.Message, []
        | CheckCommandResult.InvalidInput problem -> Some(describeInputProblem problem), []
        | CheckCommandResult.Completed(ignored, checkResult) ->
            // A file that could not be formatted is counted as changed as well as errored, so it
            // would otherwise be listed twice under two different answers.
            let failed: Set<string> = checkResult.Errors |> List.map fst |> Set.ofList

            let files: FileReport list =
                [
                    for file in ignored do
                        {
                            Path = file
                            Outcome = FileOutcome.Ignored
                        }

                    for file, error in checkResult.Errors do
                        {
                            Path = file
                            Outcome = describeFileFailure file error
                        }

                    for file in checkResult.Formatted do
                        if not (Set.contains file failed) then
                            {
                                Path = file
                                Outcome = FileOutcome.NeedsFormatting
                            }

                    for file in checkResult.Unchanged do
                        {
                            Path = file
                            Outcome = FileOutcome.Unchanged
                        }
                ]

            None, sortByPath files

    {
        Command = Command.Check
        WorkingDirectory = workingDirectory
        ExitCode = result.ExitCode
        Error = error
        Files = files
    }

let describeCommand (command: Command) : string =
    match command with
    | Command.Format -> "format"
    | Command.Check -> "check"

let describeOutcome (outcome: FileOutcome) : string =
    match outcome with
    | FileOutcome.Failed _ -> "error"
    | FileOutcome.Ignored -> "ignored"
    | FileOutcome.Formatted -> "formatted"
    | FileOutcome.Unchanged -> "unchanged"
    | FileOutcome.NeedsFormatting -> "needs-formatting"

let writeDiagnostic (json: Utf8JsonWriter) (diagnostic: Diagnostic) : unit =
    json.WriteStartObject()
    json.WriteString("severity", diagnostic.Severity)
    json.WriteString("code", diagnostic.Code)
    json.WriteString("message", diagnostic.Message)

    match diagnostic.Range with
    | None -> ()
    | Some range ->
        json.WriteStartObject "range"
        json.WriteNumber("startLine", range.StartLine)
        json.WriteNumber("startColumn", range.StartColumn)
        json.WriteNumber("endLine", range.EndLine)
        json.WriteNumber("endColumn", range.EndColumn)
        json.WriteEndObject()

    json.WriteEndObject()

// `message` and `diagnostics` appear only on a file that failed. A reader has to look at `status`
// before either means anything, and a run over a folder should not carry a null message per file
// for the thousands that were fine.
let writeFile (json: Utf8JsonWriter) (file: FileReport) : unit =
    json.WriteStartObject()
    json.WriteString("path", file.Path)
    json.WriteString("status", describeOutcome file.Outcome)

    match file.Outcome with
    | FileOutcome.Formatted
    | FileOutcome.Unchanged
    | FileOutcome.Ignored
    | FileOutcome.NeedsFormatting -> ()
    | FileOutcome.Failed(message, diagnostics) ->
        json.WriteString("message", message)
        json.WriteStartArray "diagnostics"
        List.iter (writeDiagnostic json) diagnostics
        json.WriteEndArray()

    json.WriteEndObject()

// `error` is written even when there is none, because a reader checks it on every run and a key
// that is sometimes absent is a trap in the languages most likely to be reading this.
let writeReport (json: Utf8JsonWriter) (report: RunReport) : unit =
    json.WriteStartObject()
    json.WriteNumber("version", SchemaVersion)
    json.WriteString("command", describeCommand report.Command)
    json.WriteString("workingDirectory", report.WorkingDirectory)
    json.WriteNumber("exitCode", report.ExitCode)

    match report.Error with
    | None -> json.WriteNull "error"
    | Some error -> json.WriteString("error", error)

    json.WriteStartArray "files"
    List.iter (writeFile json) report.Files
    json.WriteEndArray()
    json.WriteEndObject()

let render (report: RunReport) : string =
    // The default encoder escapes anything that could be dangerous inside an HTML document. This
    // goes to a pipe, so a path with an accent in it stays readable rather than becoming é.
    let options: JsonWriterOptions =
        JsonWriterOptions(Indented = true, Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping)

    use stream: MemoryStream = new MemoryStream()
    use json: Utf8JsonWriter = new Utf8JsonWriter(stream, options)
    writeReport json report
    json.Flush()
    Encoding.UTF8.GetString(stream.ToArray())

let reportFormatCommand (workingDirectory: string) (writer: TextWriter) (result: FormatCommandResult) : int =
    let report: RunReport = formatReport workingDirectory result
    writer.WriteLine(render report)
    report.ExitCode

let reportCheckCommand (workingDirectory: string) (writer: TextWriter) (result: CheckCommandResult) : int =
    let report: RunReport = checkReport workingDirectory result
    writer.WriteLine(render report)
    report.ExitCode
