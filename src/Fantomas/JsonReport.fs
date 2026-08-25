module Fantomas.JsonReport

open System.IO
open System.Text
open System.Text.Encodings.Web
open System.Text.Json
open Fantomas.Core
open Fantomas.FCS.Parse
open Fantomas.CommandResult
open Fantomas.DoctorCommand
open Fantomas.EditorConfig
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

let renderWith (write: Utf8JsonWriter -> unit) : string =
    // The default encoder escapes anything that could be dangerous inside an HTML document. This
    // goes to a pipe, so a path with an accent in it stays readable rather than becoming é.
    let options: JsonWriterOptions =
        JsonWriterOptions(Indented = true, Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping)

    use stream: MemoryStream = new MemoryStream()
    use json: Utf8JsonWriter = new Utf8JsonWriter(stream, options)
    write json
    json.Flush()
    Encoding.UTF8.GetString(stream.ToArray())

let render (report: RunReport) : string =
    renderWith (fun (json: Utf8JsonWriter) -> writeReport json report)

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

// The doctor document is a different shape from the other three and deliberately so. They report a
// list of files each with one outcome; this reports one file with an outcome per step, and folding
// it into `files` would put six unrelated answers under one path and lose which was which.
//
// A step the walk never reached is `null` rather than absent, for the reason every other key here
// is written whether or not it has anything in it: a key that is sometimes missing is a trap in the
// languages most likely to be reading this.

let writeDoctorFile (json: Utf8JsonWriter) (step: FileStep) : unit =
    json.WriteStartObject "file"

    match step with
    | FileStep.NotFound path ->
        json.WriteString("path", path)
        json.WriteString("status", "not-found")
    | FileStep.NotFSharp path ->
        json.WriteString("path", path)
        json.WriteString("status", "not-fsharp")
    | FileStep.Candidate file ->
        let kind: string =
            match file.Kind with
            | FileKind.Implementation -> "implementation"
            | FileKind.Signature -> "signature"
            | FileKind.Script -> "script"

        json.WriteString("path", file.Path)
        json.WriteString("status", "candidate")
        json.WriteString("kind", kind)
        json.WriteNumber("lineCount", file.LineCount)

        match file.UnreachableUnder with
        | None -> json.WriteNull "unreachableUnder"
        | Some folder -> json.WriteString("unreachableUnder", folder)

    json.WriteEndObject()

let writeIgnoreMatch (json: Utf8JsonWriter) (matched: IgnoreMatch) : unit =
    json.WriteStartObject()
    json.WriteNumber("line", matched.LineNumber)
    json.WriteString("pattern", matched.Pattern)
    json.WriteBoolean("negated", matched.Negated)
    json.WriteEndObject()

let writeDoctorIgnore (json: Utf8JsonWriter) (step: IgnoreStep option) : unit =
    match step with
    | None -> json.WriteNull "ignore"
    | Some step ->
        json.WriteStartObject "ignore"

        match step with
        | IgnoreStep.NoIgnoreFile ->
            json.WriteString("status", "no-ignore-file")
            json.WriteNull "ignoreFile"
            json.WriteStartArray "matches"
            json.WriteEndArray()
        | IgnoreStep.Governed(ignoreFile, isIgnored, matches) ->
            json.WriteString("status", (if isIgnored then "ignored" else "not-ignored"))
            json.WriteString("ignoreFile", ignoreFile)
            json.WriteStartArray "matches"
            List.iter (writeIgnoreMatch json) matches
            json.WriteEndArray()

        json.WriteEndObject()

let writeEditorConfigProblem (json: Utf8JsonWriter) (problem: EditorConfigProblem) : unit =
    json.WriteStartObject()

    match problem with
    | EditorConfigProblem.UnknownSetting setting ->
        json.WriteString("setting", setting)
        json.WriteString("status", "unknown-setting")
    | EditorConfigProblem.UnrecognizedValue(setting, value) ->
        json.WriteString("setting", setting)
        json.WriteString("status", "unrecognized-value")
        json.WriteString("value", value)

    json.WriteString("message", EditorConfigReport.describeProblem problem)
    json.WriteEndObject()

// Every setting, not only the ones an `.editorconfig` set. The text report shows the short list
// because a screen is finite; a reader that has to decide what a file will be formatted with wants
// the whole answer, and `setBy` is what tells the two apart.
let writeDoctorConfiguration (json: Utf8JsonWriter) (resolved: ResolvedConfig option) : unit =
    match resolved with
    | None -> json.WriteNull "configuration"
    | Some resolved ->
        json.WriteStartObject "configuration"

        json.WriteStartArray "editorConfigFiles"

        for file in resolved.EditorConfigFiles do
            json.WriteStringValue file

        json.WriteEndArray()

        json.WriteStartArray "problems"
        List.iter (writeEditorConfigProblem json) resolved.Problems
        json.WriteEndArray()

        json.WriteStartArray "settings"

        for setting in resolved.Settings do
            json.WriteStartObject()
            json.WriteString("setting", setting.Setting)
            json.WriteString("value", setting.Value)

            match setting.SetBy with
            | None -> json.WriteNull "setBy"
            | Some file -> json.WriteString("setBy", file)

            json.WriteEndObject()

        json.WriteEndArray()
        json.WriteEndObject()

let writeDoctorFormat (json: Utf8JsonWriter) (path: string) (step: FormatStep option) : unit =
    match step with
    | None -> json.WriteNull "format"
    | Some step ->
        json.WriteStartObject "format"

        match step with
        | FormatStep.Produced(_, FormatChange.Nothing) -> json.WriteString("status", "unchanged")
        // No line of the file changes and the file is still rewritten. `status` is what tells this
        // apart from a file that needs nothing, which is why there is no count here to read instead.
        | FormatStep.Produced(_, FormatChange.LineEndingsOnly) -> json.WriteString("status", "line-endings")
        | FormatStep.Produced(_, FormatChange.Reformatted(firstChangedLine, lineCountAfter)) ->
            json.WriteString("status", "changed")
            json.WriteNumber("firstChangedLine", firstChangedLine)
            json.WriteNumber("lineCountAfter", lineCountAfter)
        | FormatStep.Failed error ->
            json.WriteString("status", "failed")

            match describeFileFailure path error with
            | FileOutcome.Failed(message, diagnostics) ->
                json.WriteString("message", message)
                json.WriteStartArray "diagnostics"
                List.iter (writeDiagnostic json) diagnostics
                json.WriteEndArray()
            // `describeFileFailure` answers with nothing else. Named rather than swept up by a
            // wildcard, so that a case added to it has to be placed here deliberately.
            | FileOutcome.Formatted
            | FileOutcome.Unchanged
            | FileOutcome.NeedsFormatting
            | FileOutcome.Timed _ -> ()

        json.WriteEndObject()

// The diagnostics of invalid output are positions in output that was thrown away rather than in the
// file at `path`, exactly as they are for a format run, and the same warning applies: a caller
// reading them as offsets into the file on disk will be reading the wrong lines.
let writeDoctorValidity (json: Utf8JsonWriter) (step: ValidityStep option) : unit =
    match step with
    | None -> json.WriteNull "validity"
    | Some step ->
        json.WriteStartObject "validity"

        match step with
        | ValidityStep.Valid ->
            json.WriteString("status", "valid")
            json.WriteStartArray "diagnostics"
            json.WriteEndArray()
        | ValidityStep.Invalid diagnostics ->
            json.WriteString("status", "invalid")
            json.WriteStartArray "diagnostics"
            List.iter (writeDiagnostic json) (List.map describeDiagnostic diagnostics)
            json.WriteEndArray()

        json.WriteEndObject()

let writeDoctorIdempotency (json: Utf8JsonWriter) (step: IdempotencyStep option) : unit =
    match step with
    | None -> json.WriteNull "idempotency"
    | Some step ->
        json.WriteStartObject "idempotency"

        match step with
        | IdempotencyStep.Idempotent -> json.WriteString("status", "idempotent")
        | IdempotencyStep.Failed error ->
            json.WriteString("status", "failed")
            json.WriteString("message", error.Message)
        | IdempotencyStep.NotIdempotent(line, afterFirstPass, afterSecondPass) ->
            json.WriteString("status", "not-idempotent")
            json.WriteNumber("line", line)
            json.WriteString("afterFirstPass", afterFirstPass)
            json.WriteString("afterSecondPass", afterSecondPass)

        json.WriteEndObject()

let writeDoctorDocument (json: Utf8JsonWriter) (workingDirectory: string) (result: DoctorCommandResult) : unit =
    json.WriteStartObject()
    json.WriteString("command", "doctor")
    json.WriteString("workingDirectory", workingDirectory)
    json.WriteNumber("exitCode", result.ExitCode)

    let report: DoctorReport option =
        match result with
        | DoctorCommandResult.Completed report -> Some report
        | DoctorCommandResult.NotOneFile _
        | DoctorCommandResult.Failed _ -> None

    // `error` is what stopped the run before it reached the file, which for this command is a path
    // that is not one file and a failure no step could be blamed for. A file that will not format
    // is not one of them: that is what `format` is, and it is reported there.
    match result with
    | DoctorCommandResult.Failed error -> json.WriteString("error", error.Message)
    | DoctorCommandResult.NotOneFile given ->
        json.WriteString(
            "error",
            $"doctor reports on one file, and '%s{Arguments.describeInputPaths given}' is not one file."
        )
    | DoctorCommandResult.Completed _ -> json.WriteNull "error"

    match report with
    | None ->
        json.WriteNull "file"
        json.WriteNull "ignore"
        json.WriteNull "configuration"
        json.WriteNull "format"
        json.WriteNull "validity"
        json.WriteNull "idempotency"
    | Some report ->
        let path: string =
            match report.File with
            | FileStep.Candidate file -> file.Path
            | FileStep.NotFound path
            | FileStep.NotFSharp path -> path

        writeDoctorFile json report.File
        writeDoctorIgnore json report.Ignore
        writeDoctorConfiguration json report.Settings
        writeDoctorFormat json path report.Format
        writeDoctorValidity json report.Validity
        writeDoctorIdempotency json report.Idempotency

    json.WriteEndObject()

let renderDoctorReport (workingDirectory: string) (result: DoctorCommandResult) : string =
    renderWith (fun (json: Utf8JsonWriter) -> writeDoctorDocument json workingDirectory result)

let reportDoctorCommand (workingDirectory: string) (writer: TextWriter) (result: DoctorCommandResult) : int =
    writer.WriteLine(renderDoctorReport workingDirectory result)
    result.ExitCode
