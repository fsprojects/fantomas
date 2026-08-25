module Fantomas.CommandResult

/// What formatting one file came to.
[<RequireQualifiedAccess; NoComparison>]
type FormatResult =
    | Formatted of filename: string * formattedContent: string
    | Unchanged of filename: string
    | InvalidCode of filename: string * formattedContent: string
    | Error of filename: string * formattingError: exn
    | IgnoredFile of filename: string

/// What a `--check` run found. `Formatted` names the files that would change, which is the question
/// the command was asked. `Unchanged` names the ones that would not, which nothing branches on, and
/// is carried so that a report can name every file the run looked at rather than only the ones it
/// has a complaint about.
[<NoComparison>]
type CheckResult =
    {
        Errors: (string * exn) list
        Formatted: string list
        Unchanged: string list
    }

    member HasErrors: bool

    member IsValid: bool

    member NeedsFormatting: bool

/// The failure that a `FormatResult.InvalidCode` stands for. Formatting produced something that is
/// not F#, which is a bug in Fantomas rather than in the file it was given.
val invalidResultException: file: string -> Fantomas.Core.FormatException

/// A reason the input paths cannot be worked with. Both commands can end this way and both are
/// described from here, which is what keeps their wording from drifting apart.
[<RequireQualifiedAccess; Struct>]
type InputProblem =
    | UnsupportedFileType of path: string
    | NotFound of path: string
    | MultiplePathsWithOut

/// What a format run did. Turning this into text and into an exit code is `Report`'s job, so that
/// a caller can ask what happened without reading the console.
[<RequireQualifiedAccess; NoComparison>]
type FormatCommandResult =
    | InvalidInput of problem: InputProblem
    | Completed of results: FormatResult array
    /// Something was raised that no single file could be blamed for.
    | Failed of error: exn

    /// The exit code the process should end with: 1 when anything failed, 0 otherwise.
    ///
    /// This belongs to the result rather than to a reporter because there is more than one
    /// reporter, and what the process ends with cannot depend on which one printed.
    member ExitCode: int

/// What a `--check` run did. The ignored files are carried alongside, since a run that ignored
/// everything is not the same as a run that found nothing to do.
[<RequireQualifiedAccess; NoComparison>]
type CheckCommandResult =
    | InvalidInput of problem: InputProblem
    | Completed of ignored: string list * result: CheckResult
    /// Something was raised that no single file could be blamed for.
    | Failed of error: exn

    /// The exit code the process should end with: 0 when every file is already formatted, 99 when
    /// at least one needs formatting, and 1 when something failed. The two failure codes are
    /// distinct on purpose: a pipeline wants to tell "please run the formatter" apart from "this
    /// did not work".
    member ExitCode: int
