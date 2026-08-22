module Fantomas.CommandResult

open System
open Fantomas.Core

type ProfileInfo = { LineCount: int; TimeTaken: TimeSpan }

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

    member this.HasErrors = List.isNotEmpty this.Errors
    member this.NeedsFormatting = List.isNotEmpty this.Formatted
    member this.IsValid = List.isEmpty this.Errors && List.isEmpty this.Formatted

let invalidResultException (file: string) : FormatException =
    FormatException($"Formatting %s{file} leads to invalid F# code")

[<RequireQualifiedAccess>]
type InputProblem =
    | UnsupportedFileType of path: string
    | NotFound of path: string
    | NoPathGiven
    | MultiplePathsWithOut

[<RequireQualifiedAccess; NoComparison>]
type FormatCommandResult =
    | InvalidInput of problem: InputProblem
    | Completed of results: FormatResult array
    | Failed of error: exn

[<RequireQualifiedAccess; NoComparison>]
type CheckCommandResult =
    | InvalidInput of problem: InputProblem
    | Completed of ignored: string list * result: CheckResult
    | Failed of error: exn
