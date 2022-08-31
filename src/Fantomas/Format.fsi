module Fantomas.Format

open System

exception CodeFormatException of (string * Option<Exception>) array

type FormatResult =
    | Formatted of filename: string * formattedContent: string
    | Unchanged of filename: string
    | InvalidCode of filename: string * formattedContent: string
    | Error of filename: string * formattingError: Exception
    | IgnoredFile of filename: string

val formatContentAsync: (Core.FormatConfig.FormatConfig -> string -> string -> Async<FormatResult>)

val formatFileAsync: (string -> Async<FormatResult>)

type CheckResult =
    { Errors: (string * exn) list
      Formatted: string list }

    member HasErrors: bool

    member IsValid: bool

    member NeedsFormatting: bool

/// Runs a check on the given files and reports the result to the given output:
///
/// * It shows the paths of the files that need formatting
/// * It shows the path and the error message of files that failed the format check
///
/// Returns:
///
/// A record with the file names that were formatted and the files that encounter problems while formatting.
val checkCode: filenames: seq<string> -> Async<CheckResult>
