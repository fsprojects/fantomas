module Fantomas.JsonReport

open System.IO
open Fantomas.CommandResult

/// The version of the document this module writes, carried in the document itself so a caller can
/// tell one shape from the next. It goes up when a key changes meaning or leaves, not when one is
/// added: a reader that ignores what it does not recognise keeps working across additions.
[<Literal>]
val SchemaVersion: int = 1

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

/// What became of one file. `Formatted` only ever comes from a format run and `NeedsFormatting` only
/// from a `--check` run, because a check writes nothing and a format run leaves nothing needing it.
/// The other three come from either.
[<RequireQualifiedAccess; NoComparison>]
type FileOutcome =
    | Formatted
    | Unchanged
    | Ignored
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
