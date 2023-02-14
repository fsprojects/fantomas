module Fantomas.Format

open System
open System.IO
open Fantomas.Core

type ProfileInfo = { LineCount: int; TimeTaken: TimeSpan }

type FormatResult =
    | Formatted of filename: string * formattedContent: string * profileInfo: ProfileInfo option
    | Unchanged of filename: string * profileInfo: ProfileInfo option
    | InvalidCode of filename: string * formattedContent: string
    | Error of filename: string * formattingError: Exception
    | IgnoredFile of filename: string

let private formatContentInternalAsync
    (compareWithoutLineEndings: bool)
    (config: FormatConfig)
    (profile: bool)
    (file: string)
    (originalContent: string)
    : Async<FormatResult> =
    if IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) file then
        async { return IgnoredFile file }
    else
        async {
            try
                let isSignatureFile = Path.GetExtension(file) = ".fsi"

                let! { Code = formattedContent }, profileInfo =
                    if profile then
                        async {
                            let sw = Diagnostics.Stopwatch.StartNew()
                            let! res = CodeFormatter.FormatDocumentAsync(isSignatureFile, originalContent, config)
                            sw.Stop()

                            let count =
                                originalContent.Length - originalContent.Replace(Environment.NewLine, "").Length

                            let profileInfo =
                                { LineCount = count
                                  TimeTaken = sw.Elapsed }

                            return res, Some profileInfo
                        }
                    else
                        async {
                            let! res = CodeFormatter.FormatDocumentAsync(isSignatureFile, originalContent, config)
                            return res, None
                        }

                let contentChanged =
                    if compareWithoutLineEndings then
                        let stripNewlines (s: string) =
                            System.Text.RegularExpressions.Regex.Replace(s, @"\r", String.Empty)

                        (stripNewlines originalContent) <> (stripNewlines formattedContent)
                    else
                        originalContent <> formattedContent

                if contentChanged then
                    let! isValid = CodeFormatter.IsValidFSharpCodeAsync(isSignatureFile, formattedContent)

                    if not isValid then
                        return InvalidCode(filename = file, formattedContent = formattedContent)
                    else
                        return
                            Formatted(filename = file, formattedContent = formattedContent, profileInfo = profileInfo)
                else
                    return Unchanged(filename = file, profileInfo = profileInfo)
            with ex ->
                return Error(file, ex)
        }

let formatContentAsync = formatContentInternalAsync false

let private formatFileInternalAsync (compareWithoutLineEndings: bool) (profile: bool) (file: string) =
    let config = EditorConfig.readConfiguration file

    if IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) file then
        async { return IgnoredFile file }
    else

        async {
            let! originalContent = File.ReadAllTextAsync file |> Async.AwaitTask

            let! formatted =
                originalContent
                |> formatContentInternalAsync compareWithoutLineEndings config profile file

            return formatted
        }

let formatFileAsync = formatFileInternalAsync false

type CheckResult =
    { Errors: (string * exn) list
      Formatted: string list }

    member this.HasErrors = List.isNotEmpty this.Errors
    member this.NeedsFormatting = List.isNotEmpty this.Formatted
    member this.IsValid = List.isEmpty this.Errors && List.isEmpty this.Formatted

/// Runs a check on the given files and reports the result to the given output:
///
/// * It shows the paths of the files that need formatting
/// * It shows the path and the error message of files that failed the format check
///
/// Returns:
///
/// A record with the file names that were formatted and the files that encounter problems while formatting.
let checkCode (filenames: seq<string>) =
    async {
        let! formatted =
            filenames
            |> Seq.filter (IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) >> not)
            |> Seq.map (formatFileInternalAsync true false)
            |> Async.Parallel

        let getChangedFile =
            function
            | FormatResult.Unchanged _
            | FormatResult.IgnoredFile _ -> None
            | FormatResult.Formatted(f, _, _)
            | FormatResult.Error(f, _)
            | FormatResult.InvalidCode(f, _) -> Some f

        let changes = formatted |> Seq.choose getChangedFile |> Seq.toList

        let getErrors =
            function
            | FormatResult.Error(f, e) -> Some(f, e)
            | _ -> None

        let errors = formatted |> Seq.choose getErrors |> Seq.toList

        return { Errors = errors; Formatted = changes }
    }
