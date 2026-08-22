module Fantomas.CommandResult

open Fantomas

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
    | IgnoredFile of file: string
    | Completed of results: FormatResult array
    /// Something was raised that no single file could be blamed for.
    | Failed of error: exn

/// What a `--check` run did.
[<RequireQualifiedAccess; NoComparison>]
type CheckCommandResult =
    | InvalidInput of problem: InputProblem
    | IgnoredFile of file: string
    | Completed of result: CheckResult
