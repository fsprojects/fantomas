module Fantomas.JsonReport

open System.IO
open Fantomas.CommandResult

// The document carries no version, and that is the promise rather than an omission.
//
// A version number says a shape is a contract somebody is maintaining, and this one is not. It
// exists so that a machine reading a run can see what happened, which is a job that tolerates the
// shape moving; the cost of the alternative is holding a key nobody uses until the next major
// because one script somewhere parsed it. What is written here may change in any release, the help
// page says so beside the flag, and a reader that needs a promise wants the exit code, which has
// one.

/// Where in a file a diagnostic points. Lines and columns are both one based, matching what the F#
/// compiler prints for the same file and what the text report writes.
type Range =
    {
        StartLine: int
        StartColumn: int
        EndLine: int
        EndColumn: int
    }

/// One thing the parser had to say about a file. `Severity` is `error`, `warning` or `info`, and
/// `Code` is the compiler's number for it in the `FSnnnn` form, `FS0000` when it has none.
///
/// A parse failure carries every diagnostic the parser produced, warnings included, because a
/// warning is often what explains the error next to it.
type Diagnostic =
    {
        Severity: string
        Code: string
        Message: string
        Range: Range option
    }

/// What became of one file that was looked at. A file an ignore file kept the run away from is not
/// one of these and is not counted anywhere either: see `RunReport`.
///
/// `Formatted` only ever comes from a format run and `NeedsFormatting` only from a `--check` run,
/// because a check writes nothing and a format run leaves nothing needing it.
[<RequireQualifiedAccess; NoComparison>]
type FileOutcome =
    | Formatted
    | Unchanged
    | NeedsFormatting
    /// Only from a `profile` run, which is the only command that measures. The milliseconds are the
    /// file alone, and the run's own total is on the report.
    | Timed of lineCount: int * defineCombinations: int * milliseconds: int
    | Failed of message: string * diagnostics: Diagnostic list

[<NoComparison>]
type FileReport = { Path: string; Outcome: FileOutcome }

/// Which command the document describes, which is what tells `needs-formatting` and `formatted`
/// apart from each other. Both commands list every file they looked at.
[<RequireQualifiedAccess; Struct>]
type Command =
    | Format
    | Check
    | Profile

/// One run, as a document. `Error` is what stopped the run before any file was reached, such as an
/// input path that does not exist, and is separate from a file that failed on its own.
///
/// Nothing here says what an ignore file kept the run away from, and that is deliberate rather than
/// an omission. There is no honest number for it. A pattern that names a file can be counted,
/// because the file is found and then set aside; a pattern that names a folder cannot, because the
/// folder is never opened and what is inside it is as unknown as what is inside a folder that is
/// not there. A count right about the first and blind to the second reads as though it covered
/// both, and this repository's own `.fantomasignore` names three folders holding ninety six F#
/// files. So `files` is what the run looked at, and what it did not is a question for a command
/// that can afford to open the folder and answer it properly.
///
/// A file's path is the one the run was given, which is usually relative, and `WorkingDirectory` is
/// what it is relative to. The absolute path is the two joined. They are carried apart rather than
/// resolved per file because the document is read by a machine paying for every token of it, and a
/// run over a thousand files would otherwise repeat the same prefix a thousand times.
[<NoComparison>]
type RunReport =
    {
        Command: Command
        WorkingDirectory: string
        ExitCode: int
        Error: string option
        /// How long the whole run took, on the command that measures and absent on the two that do
        /// not. Not the sum of the files: reading each one and walking the folder are in here and
        /// in none of them.
        ElapsedMilliseconds: int option
        Files: FileReport list
    }

/// What a format run did: every file it looked at, ordered by path. `workingDirectory` is the folder
/// the paths are relative to, which for the tool is the folder it was run in.
val formatReport: workingDirectory: string -> result: FormatCommandResult -> RunReport

/// What a `--check` run found: every file it looked at, ordered by path, the same way a format run
/// reports. A file that is already formatted is `Unchanged`, rather than being left out and read
/// from its absence.
///
/// A file that failed is reported as failed and not also as needing formatting, although the check
/// counts it as both: one file is one entry.
val checkReport: workingDirectory: string -> result: CheckCommandResult -> RunReport

/// What a `profile` run measured: every file it timed, ordered by path like the other two, rather
/// than slowest first the way the text report orders them. A reader that wants them by time can
/// sort them; a reader looking one file up should not have to.
val profileReport: workingDirectory: string -> result: ProfileCommand.ProfileCommandResult -> RunReport

/// Render a report as the JSON text to write, indented, without a trailing newline.
val render: report: RunReport -> string

/// Write what a format run did to `writer` as one JSON document, and return the exit code the
/// process should end with. Nothing is logged: the document is the whole report.
val reportFormatCommand: workingDirectory: string -> writer: TextWriter -> result: FormatCommandResult -> int

/// Write what a `--check` run found to `writer` as one JSON document, and return the exit code the
/// process should end with. Nothing is logged: the document is the whole report.
val reportCheckCommand: workingDirectory: string -> writer: TextWriter -> result: CheckCommandResult -> int

/// Write what a `profile` run measured to `writer` as one JSON document, and return the exit code
/// the process should end with. Nothing is logged: the document is the whole report.
val reportProfileCommand:
    workingDirectory: string -> writer: TextWriter -> result: ProfileCommand.ProfileCommandResult -> int
