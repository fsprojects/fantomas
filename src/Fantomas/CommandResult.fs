module Fantomas.CommandResult

open System
open Fantomas.Core

type ProfileInfo = { LineCount: int; TimeTaken: TimeSpan }

[<RequireQualifiedAccess; NoComparison>]
type FormatResult =
    | Formatted of filename: string * formattedContent: string * profileInfo: ProfileInfo option
    | Unchanged of filename: string * profileInfo: ProfileInfo option
    | InvalidCode of filename: string * formattedContent: string
    | Error of filename: string * formattingError: exn
    | IgnoredFile of filename: string

[<NoComparison>]
type CheckResult =
    {
        Errors: (string * exn) list
        Formatted: string list
        Unchanged: string list
    }

    member this.HasErrors = List.isNotEmpty this.Errors
    member this.NeedsFormatting = List.isNotEmpty this.Formatted
    member this.IsValid = List.isEmpty this.Errors && List.isEmpty this.Formatted

let invalidResultException (file: string) : FormatException =
    FormatException($"Formatting %s{file} leads to invalid F# code")

[<RequireQualifiedAccess; Struct>]
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

    member this.ExitCode: int =
        let failed (result: FormatResult) : bool =
            match result with
            | FormatResult.Error _
            | FormatResult.InvalidCode _ -> true
            | FormatResult.Formatted _
            | FormatResult.Unchanged _
            | FormatResult.IgnoredFile _ -> false

        match this with
        | FormatCommandResult.InvalidInput _
        | FormatCommandResult.Failed _ -> 1
        | FormatCommandResult.Completed results -> if Array.exists failed results then 1 else 0

[<RequireQualifiedAccess; NoComparison>]
type CheckCommandResult =
    | InvalidInput of problem: InputProblem
    | Completed of ignored: string list * result: CheckResult
    | Failed of error: exn

    member this.ExitCode: int =
        match this with
        | CheckCommandResult.InvalidInput _
        | CheckCommandResult.Failed _ -> 1
        | CheckCommandResult.Completed(_, checkResult) ->
            if checkResult.IsValid then 0
            elif checkResult.HasErrors then 1
            else 99
