namespace Fantomas

open System
open System.IO
open Fantomas.Core

type ProfileInfo = { LineCount: int; TimeTaken: TimeSpan }

[<RequireQualifiedAccess; NoComparison>]
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

    static member Create(config: FormatConfig, compareWithoutLineEndings: bool, profile: bool, file: string) =
        { Config = config
          CompareWithoutLineEndings = compareWithoutLineEndings
          Profile = profile
          File = file }

    static member Create(compareWithoutLineEndings: bool, profile: bool, file: string) =
        { Config = EditorConfig.readConfiguration file
          CompareWithoutLineEndings = compareWithoutLineEndings
          Profile = profile
          File = file }

[<NoComparison>]
type CheckResult =
    { Errors: (string * exn) list
      Formatted: string list }

    member this.HasErrors = List.isNotEmpty this.Errors
    member this.NeedsFormatting = List.isNotEmpty this.Formatted
    member this.IsValid = List.isEmpty this.Errors && List.isEmpty this.Formatted

module Format =

    let private formatContentInternalAsync
        (formatParams: FormatParams)
        (originalContent: string)
        : Async<FormatResult> =
        if IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) formatParams.File then
            async { return FormatResult.IgnoredFile formatParams.File }
        else
            async {
                try
                    let isSignatureFile = Path.GetExtension(formatParams.File) = ".fsi"

                    let! { Code = formattedContent }, profileInfo =
                        if formatParams.Profile then
                            async {
                                let sw = Diagnostics.Stopwatch.StartNew()

                                let! res =
                                    CodeFormatter.FormatDocumentAsync(
                                        isSignatureFile,
                                        originalContent,
                                        formatParams.Config
                                    )

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
                                let! res =
                                    CodeFormatter.FormatDocumentAsync(
                                        isSignatureFile,
                                        originalContent,
                                        formatParams.Config
                                    )

                                return res, None
                            }

                    let contentChanged =
                        if formatParams.CompareWithoutLineEndings then
                            let stripNewlines (s: string) =
                                System.Text.RegularExpressions.Regex.Replace(s, @"\r", String.Empty)

                            (stripNewlines originalContent) <> (stripNewlines formattedContent)
                        else
                            originalContent <> formattedContent

                    if contentChanged then
                        let! isValid = CodeFormatter.IsValidFSharpCodeAsync(isSignatureFile, formattedContent)

                        if not isValid then
                            return
                                FormatResult.InvalidCode(
                                    filename = formatParams.File,
                                    formattedContent = formattedContent
                                )
                        else
                            return
                                FormatResult.Formatted(
                                    filename = formatParams.File,
                                    formattedContent = formattedContent,
                                    profileInfo = profileInfo
                                )
                    else
                        return FormatResult.Unchanged(filename = formatParams.File, profileInfo = profileInfo)
                with ex ->
                    return FormatResult.Error(formatParams.File, ex)
            }

    let formatContentAsync = formatContentInternalAsync

    let private formatFileInternalAsync (parms: FormatParams) =
        if IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) parms.File then
            async { return FormatResult.IgnoredFile parms.File }
        else

            async {
                let! originalContent = File.ReadAllTextAsync parms.File |> Async.AwaitTask

                let! formatted = originalContent |> formatContentInternalAsync parms

                return formatted
            }

    let formatFileAsync = formatFileInternalAsync

    /// Runs a check on the given files and reports the result to the given output:
    ///
    /// * It shows the paths of the files that need formatting
    /// * It shows the path and the error message of files that failed the format check
    ///
    /// Returns:
    ///
    /// A record with the file names that were formatted and the files that encounter problems while formatting.
    let checkCode (filenames: string seq) =
        async {
            let! formatted =
                filenames
                |> Seq.choose (fun filename ->
                    if IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) filename then
                        None
                    else
                        Some(formatFileInternalAsync (FormatParams.Create(true, false, filename))))
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
