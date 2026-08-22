module Fantomas.CommandResult

open System

type ProfileInfo = { LineCount: int; TimeTaken: TimeSpan }

/// What formatting one file came to.
[<RequireQualifiedAccess; NoComparison>]
type FormatResult =
    | Formatted of filename: string * formattedContent: string * profileInfo: ProfileInfo option
    | Unchanged of filename: string * profileInfo: ProfileInfo option
    | InvalidCode of filename: string * formattedContent: string
    | Error of filename: string * formattingError: Exception
    | IgnoredFile of filename: string

[<NoComparison>]
type CheckResult =
    { Errors: (string * exn) list
      Formatted: string list }

    member HasErrors: bool

    member IsValid: bool

    member NeedsFormatting: bool

/// The failure that a `FormatResult.InvalidCode` stands for. Formatting produced something that is
/// not F#, which is a bug in Fantomas rather than in the file it was given.
val invalidResultException: file: string -> Fantomas.Core.FormatException

/// A reason the input paths cannot be worked with. Both commands can end this way and both are
/// described from here, which is what keeps their wording from drifting apart.
[<RequireQualifiedAccess>]
type InputProblem =
    | UnsupportedFileType of path: string
    | NotFound of path: string
    | NoPathGiven
    | MultiplePathsWithOut

/// What a format run did. Turning this into text and into an exit code is `Report`'s job, so that
/// a caller can ask what happened without reading the console.
[<RequireQualifiedAccess; NoComparison>]
type FormatCommandResult =
    | InvalidInput of problem: InputProblem
    | Completed of results: FormatResult array
    /// Something was raised that no single file could be blamed for.
    | Failed of error: exn

/// What a `--check` run did. The ignored files are carried alongside, since a run that ignored
/// everything is not the same as a run that found nothing to do.
[<RequireQualifiedAccess; NoComparison>]
type CheckCommandResult =
    | InvalidInput of problem: InputProblem
    | Completed of ignored: string list * result: CheckResult
    /// Something was raised that no single file could be blamed for.
    | Failed of error: exn
