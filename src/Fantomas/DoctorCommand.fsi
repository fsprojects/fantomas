module Fantomas.DoctorCommand

open Fantomas.FCS.Parse
open Fantomas
open Fantomas.Arguments
open Fantomas.Cli
open Fantomas.EditorConfig

/// The lines of a source file, however it ends its lines, and without an empty last line for the
/// newline that terminates the last one. A file that ends in a newline has as many lines as one
/// that does not, and a file with no newline at all still has the line it holds.
val lines: content: string -> string array

/// The first line at which two files differ, counting from one, or `None` when they do not differ.
/// A file that is the other with lines added to it differs at the first line only one of them has.
val firstDifference: left: string array -> right: string array -> int option

/// What Fantomas takes a file to be, which its extension decides and which decides how it is
/// parsed.
///
/// `.fsi` is the only extension parsed as a signature. `.mli` is accepted and parsed as an
/// implementation, here as everywhere else in the tool, so it is named as one: this reports what
/// Fantomas does with a file rather than what its extension suggests elsewhere.
[<RequireQualifiedAccess; Struct>]
type FileKind =
    | Implementation
    | Signature
    | Script

/// What the path turned out to name. The first question, and the one every other question is asked
/// of a file that answers it.
[<RequireQualifiedAccess; NoComparison>]
type FileStep =
    /// Absolute, like every other path this command reports. The path as it was typed is what the
    /// reader already has; resolving it is what tells them they are asking from the wrong folder,
    /// which is what a path that is not there usually means.
    | NotFound of path: string
    /// A file that is there and is not one Fantomas formats.
    | NotFSharp of path: string
    | Candidate of file: DoctorFile

/// An F# file Fantomas can be asked about.
and [<NoComparison>] DoctorFile =
    {
        /// Absolute, because everything below it is: an ignore file and an `.editorconfig` are
        /// resolved by walking up from here, and a path relative to a working directory the reader
        /// may not be in is not something they can follow.
        Path: string
        Kind: FileKind
        LineCount: int
        /// The folder a walk would refuse to open, when the file sits under one a compiler or a
        /// package manager wrote. Naming the file itself still formats it, so this is a note about
        /// why a run over the tree above it does nothing, rather than a reason to stop.
        UnreachableUnder: string option
    }

/// What `.fantomasignore` says about the file.
[<RequireQualifiedAccess; NoComparison>]
type IgnoreStep =
    /// No `.fantomasignore` at or above the file, so nothing could have skipped it. Worth saying
    /// out loud: it is an answer, and it is the one somebody looking for the wrong ignore file
    /// needs.
    | NoIgnoreFile
    /// The nearest `.fantomasignore` at or above the file, what it decided, and every line of it
    /// whose pattern matches. The last of those lines is the one that decided.
    | Governed of ignoreFile: string * isIgnored: bool * matches: IgnoreMatch list

/// How the formatted text differs from the file, decided the way a format run decides whether to
/// write: by comparing the text as it is. A `check` run is the one that overlooks a difference in
/// line endings, and this reports what formatting would do rather than what checking would say.
[<RequireQualifiedAccess; Struct>]
type FormatChange =
    | Nothing
    /// The text is the same line for line and the file would still be rewritten, because the line
    /// endings are not what the configuration asks for. The state that reads as "already
    /// formatted" to everything that compares line by line, and is why a working tree checked out
    /// with the other platform's endings looks untouched right up until it is formatted.
    | LineEndingsOnly
    /// Where the file and the result first part, counting from one, and how many lines the result
    /// has.
    ///
    /// Two exact facts rather than one count of what changed. Counting lines that differ by
    /// position is not a count of edits: splitting one long line into five moves every line below
    /// it, and a file of five lines came back as `9 lines of 5 would change`, which is a number a
    /// reader can see is wrong. Saying where the first change is and how long the file becomes
    /// answers what somebody wants without claiming to have diffed anything.
    | Reformatted of firstChangedLine: int * lineCountAfter: int

/// What formatting the file produced.
[<RequireQualifiedAccess; NoComparison>]
type FormatStep =
    | Failed of error: exn
    /// Formatting ran. Carries what it produced, because everything after this step is asked about
    /// that text and it is written nowhere else.
    | Produced of formatted: string * change: FormatChange

/// Whether Fantomas will accept what it produced. It always should, and when it does not that is a
/// bug in Fantomas rather than a problem with the file.
[<RequireQualifiedAccess; NoComparison>]
type ValidityStep =
    | Valid
    | Invalid of diagnostics: FSharpParserDiagnostic list

/// Whether formatting the result again leaves it alone. It should, and when it does not the file
/// will keep changing under a formatter that is run twice.
[<RequireQualifiedAccess; NoComparison>]
type IdempotencyStep =
    | Idempotent
    /// Carries the line at which the two passes part, counting from one, and that line from each
    /// of them, so the report can put the pair in front of the reader rather than send them to
    /// reproduce it.
    | NotIdempotent of line: int * afterFirstPass: string * afterSecondPass: string
    /// The second pass failed on output the first pass produced, which is a failure of Fantomas on
    /// its own text.
    | Failed of error: exn

/// Everything Fantomas would do to one file, in the order it does it.
///
/// A step is `None` when the walk stopped before reaching it, which is what an ignored file, a
/// failed format and output Fantomas would not accept all lead to. That is deliberately not the
/// same as a step with nothing to say: the report tells the reader which it is, because "nothing
/// went wrong here" and "this was never asked" are different answers.
[<NoComparison>]
type DoctorReport =
    {
        File: FileStep
        Ignore: IgnoreStep option
        Settings: ResolvedConfig option
        Format: FormatStep option
        Validity: ValidityStep option
        Idempotency: IdempotencyStep option
    }

/// What a doctor run came to.
[<RequireQualifiedAccess; NoComparison>]
type DoctorCommandResult =
    /// `doctor` traces one file through the steps Fantomas puts it through, so a folder and a list
    /// of paths are not questions it can answer. Every other command takes both, which is why this
    /// says so rather than picking one of them.
    | NotOneFile of given: InputPath
    | Completed of report: DoctorReport
    /// Something was raised that no step could be blamed for.
    | Failed of error: exn

    /// The exit code the process should end with: 1 when the path could not be diagnosed at all,
    /// or when a step failed, and 0 otherwise.
    ///
    /// A file that `.fantomasignore` matches is 0. So is a file that needs formatting: this
    /// command reports what would happen and `check` is the one that fails over it. What is left
    /// exiting 1 is a path that is not there, a file that would not format, output Fantomas would
    /// not accept and a second pass that changed the first, all of which are things somebody has
    /// to act on.
    member ExitCode: int

/// Everything from formatting onwards, which is the half of the walk that is about the text rather
/// than about the file system around it. Each step gates the one after it: there is nothing to
/// validate when formatting failed, and formatting output Fantomas has already refused a second
/// time reports a parse failure in text nobody can open.
///
/// `format` is what turns source into formatted source, which for a run is `CodeFormatter` under
/// the file's configuration. It is taken as a function rather than reached for, because the three
/// things this can find are all failures of Fantomas on its own output, and there is no F# anybody
/// can write that makes a correct formatter produce them. Without a way to hand over a formatter
/// that does, the reports for output Fantomas will not accept and for a second pass that disagrees
/// with the first are code nothing has ever run.
///
/// Whether the output parses is still asked of the real parser, whatever `format` returned. That
/// is the question the step exists to ask, and a stubbed answer to it would be worth nothing.
val walkFormatting:
    report: DoctorReport -> isSignature: bool -> format: (string -> string) -> content: string -> DoctorReport

/// Walk one file through everything Fantomas does to it and report what happened at each step:
/// whether the file is there and is one Fantomas formats, which `.fantomasignore` governs it and
/// which line of it decided, which settings apply and where each came from, what formatting
/// produced, whether Fantomas accepts its own output, and whether formatting that output again
/// leaves it alone.
///
/// Nothing is written, which is what makes it safe against a working tree that has not been
/// committed, and that is when it gets reached for. That is `profile`'s rule and for the same
/// reason.
///
/// Every step gates the one after it. A file `.fantomasignore` matches stops the walk, because
/// what Fantomas does to that file is nothing, and formatting it anyway would answer a question
/// the reader did not ask with a report that contradicts the run they are trying to explain.
val runDoctorCommand: env: CliEnvironment -> inputPath: InputPath -> DoctorCommandResult
