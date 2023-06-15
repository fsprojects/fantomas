namespace Fantomas

open System
open System.IO.Abstractions
open Fantomas.Core

type ProfileInfo = { LineCount: int; TimeTaken: TimeSpan }

type FormatResult =
    | Formatted of filename: string * formattedContent: string * profileInfo: ProfileInfo option
    | Unchanged of filename: string * profileInfo: ProfileInfo option
    | InvalidCode of filename: string * formattedContent: string
    | Error of filename: string * formattingError: Exception

type FormatParams =
    { Config: FormatConfig
      CompareWithoutLineEndings: bool
      Profile: bool
      File: string }

    static member Create: bool * bool * string -> FormatParams
    static member Create: FormatConfig * bool * bool * string -> FormatParams

type CheckResult =
    { Errors: (string * exn) list
      Formatted: string list }

    member HasErrors: bool

    member IsValid: bool

    member NeedsFormatting: bool

module Format =
    val formatContentAsync: (FormatParams -> string -> Async<FormatResult>)

    val formatFileAsync: (FormatParams -> Async<FormatResult>)

    /// <summary>Runs a check on the given files and reports the result to the given output</summary>
    /// <remarks>
    /// * It shows the paths of the files that need formatting
    /// * It shows the path and the error message of files that failed the format check
    /// </remarks>
    /// <returns>A record with the file names that were formatted and the files that encounter problems while formatting.</returns>
    val checkCode: filenames: seq<IFileInfo> -> Async<CheckResult>
