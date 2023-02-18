module Fantomas.Format

open System
open Fantomas.Core

type ProfileInfo = { LineCount: int; TimeTaken: TimeSpan }

type FormatResult =
    | Formatted of filename: string * formattedContent: string * profileInfo: ProfileInfo option
    | Unchanged of filename: string * profileInfo: ProfileInfo option
    | InvalidCode of filename: string * formattedContent: string
    | Error of filename: string * formattingError: Exception
    | IgnoredFile of filename: string

type FormatParams =
    { Config: FormatConfig
      CompareWithoutLineEndings: bool
      Profile: bool
      File: string }

    static member Create: bool * bool * string -> FormatParams
    static member Create: FormatConfig * bool * bool * string -> FormatParams

val formatContentAsync: (FormatParams -> string -> Async<FormatResult>)

val formatFileAsync: (FormatParams -> Async<FormatResult>)

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
