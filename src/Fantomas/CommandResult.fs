module Fantomas.CommandResult

open Fantomas

[<RequireQualifiedAccess>]
type InputProblem =
    | UnsupportedFileType of path: string
    | NotFound of path: string
    | NoPathGiven
    | MultiplePathsWithOut

[<RequireQualifiedAccess; NoComparison>]
type FormatCommandResult =
    | InvalidInput of problem: InputProblem
    | IgnoredFile of file: string
    | Completed of results: FormatResult array
    | Failed of error: exn

[<RequireQualifiedAccess; NoComparison>]
type CheckCommandResult =
    | InvalidInput of problem: InputProblem
    | IgnoredFile of file: string
    | Completed of result: CheckResult
