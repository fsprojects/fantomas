module Fantomas.JsonReport

open System.IO
open System.Text
open System.Text.Encodings.Web
open System.Text.Json
open Fantomas.Core
open Fantomas.FCS.Parse
open Fantomas.CommandResult
open Fantomas.Report

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
    | NeedsFormatting
    | Timed of lineCount: int * defineCombinations: int * milliseconds: int
    | Failed of message: string * diagnostics: Diagnostic list

[<NoComparison>]
type FileReport = { Path: string; Outcome: FileOutcome }

[<RequireQualifiedAccess; Struct>]
type Command =
    | Format
    | Check
    | Profile

[<NoComparison>]
type RunReport =
    {
        Command: Command
        WorkingDirectory: string
        ExitCode: int
        Error: string option
        ElapsedMilliseconds: int option
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

// The two failures with positions worth taking apart, since those are what a caller can act on
// without opening the file. Everything else has only a message, so it is carried as one.
//
// The positions of an invalid output are positions in output that was thrown away rather than in
// the file at `path`, which the text report says out loud and this document has nowhere to. A
// caller reading these as offsets into the file on disk will be reading the wrong lines. What the
// document is for is knowing that it happened and where to look; the text report is what says what
// the lines are.
let describeFileFailure (file: string) (error: exn) : FileOutcome =
    match error with
    | :? ParseException as parseFailure ->
        FileOutcome.Failed(
            $"%s{file} could not be parsed by Fantomas",
            List.map describeDiagnostic parseFailure.Diagnostics
        )
    | :? InvalidCodeException as invalid ->
        FileOutcome.Failed(invalid.Message, List.map describeDiagnostic invalid.Diagnostics)
    | _ ->
        let message: string = describeFailure error |> Option.defaultValue error.Message

        FileOutcome.Failed(message, [])

// The order the files were worked on is the order a folder walk happened to return them in, which
// is neither the order they were given nor one worth promising. Sorting is a rule that can be
// stated, and it holds for both commands, where the check report has three lists to merge anyway.
let sortByPath (files: FileReport list) : FileReport list =
    List.sortBy (fun (file: FileReport) -> file.Path) files

let describeResult (result: FormatResult) : FileReport option =
    match result with
    // Neither listed nor counted. See `RunReport`.
    | FormatResult.IgnoredFile _ -> None
    | FormatResult.Unchanged file ->
        Some
            {
                Path = file
                Outcome = FileOutcome.Unchanged
            }
    | FormatResult.Formatted(file, _) ->
        Some
            {
                Path = file
                Outcome = FileOutcome.Formatted
            }
    | FormatResult.Error(file, error) ->
        Some
            {
                Path = file
                Outcome = describeFileFailure file error
            }
    | FormatResult.InvalidCode(file, formattedContent, diagnostics) ->
        Some
            {
                Path = file
                Outcome = describeFileFailure file (InvalidCodeException(formattedContent, diagnostics))
            }

let formatReport (workingDirectory: string) (result: FormatCommandResult) : RunReport =
    let error, files: string option * FileReport list =
        match result with
        | FormatCommandResult.Failed error -> Some error.Message, []
        | FormatCommandResult.InvalidInput problem -> Some(describeInputProblem problem), []
        | FormatCommandResult.Completed results ->
            None, results |> Array.choose describeResult |> List.ofArray |> sortByPath

    {
        Command = Command.Format
        WorkingDirectory = workingDirectory
        ExitCode = result.ExitCode
        Error = error
        ElapsedMilliseconds = None
        Files = files
    }

let checkReport (workingDirectory: string) (result: CheckCommandResult) : RunReport =
    let error, files: string option * FileReport list =
        match result with
        | CheckCommandResult.Failed error -> Some error.Message, []
        | CheckCommandResult.InvalidInput problem -> Some(describeInputProblem problem), []
        | CheckCommandResult.Completed(_, checkResult) ->
            // A file that could not be formatted is counted as changed as well as errored, so it
            // would otherwise be listed twice under two different answers.
            let failed: Set<string> = checkResult.Errors |> List.map fst |> Set.ofList

            let files: FileReport list =
                [
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
        ElapsedMilliseconds = None
        Files = files
    }

let profileReport (workingDirectory: string) (result: ProfileCommand.ProfileCommandResult) : RunReport =
    let error, elapsed, files: string option * int option * FileReport list =
        match result with
        | ProfileCommand.ProfileCommandResult.Failed error -> Some error.Message, None, []
        | ProfileCommand.ProfileCommandResult.InvalidInput problem -> Some(describeInputProblem problem), None, []
        | ProfileCommand.ProfileCommandResult.Completed profile ->
            let files: FileReport list =
                [
                    for file, error in profile.Errors do
                        {
                            Path = file
                            Outcome = describeFileFailure file error
                        }

                    for timing in profile.Timings do
                        {
                            Path = timing.File
                            Outcome =
                                FileOutcome.Timed(
                                    timing.LineCount,
                                    timing.DefineCombinations,
                                    int (round timing.TimeTaken.TotalMilliseconds)
                                )
                        }
                ]

            None, Some(int (round profile.Elapsed.TotalMilliseconds)), sortByPath files

    {
        Command = Command.Profile
        WorkingDirectory = workingDirectory
        ExitCode = result.ExitCode
        Error = error
        ElapsedMilliseconds = elapsed
        Files = files
    }

let describeCommand (command: Command) : string =
    match command with
    | Command.Format -> "format"
    | Command.Check -> "check"
    | Command.Profile -> "profile"

let describeOutcome (outcome: FileOutcome) : string =
    match outcome with
    | FileOutcome.Failed _ -> "error"
    | FileOutcome.Formatted -> "formatted"
    | FileOutcome.Unchanged -> "unchanged"
    | FileOutcome.NeedsFormatting -> "needs-formatting"
    | FileOutcome.Timed _ -> "timed"

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
    | FileOutcome.NeedsFormatting -> ()
    | FileOutcome.Timed(lineCount, defineCombinations, milliseconds) ->
        json.WriteNumber("lineCount", lineCount)
        json.WriteNumber("defineCombinations", defineCombinations)
        json.WriteNumber("milliseconds", milliseconds)
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
    json.WriteString("command", describeCommand report.Command)
    json.WriteString("workingDirectory", report.WorkingDirectory)
    json.WriteNumber("exitCode", report.ExitCode)

    match report.Error with
    | None -> json.WriteNull "error"
    | Some error -> json.WriteString("error", error)

    // Only on the command that measures. The other two would carry a null on every run to say
    // nothing, which is what the per file keys already avoid.
    match report.ElapsedMilliseconds with
    | None -> ()
    | Some elapsed -> json.WriteNumber("elapsedMilliseconds", elapsed)

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

let reportProfileCommand
    (workingDirectory: string)
    (writer: TextWriter)
    (result: ProfileCommand.ProfileCommandResult)
    : int
    =
    writer.WriteLine(render (profileReport workingDirectory result))
    result.ExitCode
