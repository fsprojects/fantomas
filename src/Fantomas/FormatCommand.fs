module Fantomas.FormatCommand

open System
open System.IO
open System.IO.Abstractions
open System.Text
open Fantomas.Core
open Fantomas
open Fantomas.Logging
open Fantomas.Arguments
open Fantomas.Cli
open Fantomas.CommandResult
open Fantomas.Paths
open Fantomas.Plan
open Fantomas.Theme

type FormatParams =
    {
        Config: FormatConfig
        CompareWithoutLineEndings: bool
        File: string
    }

    static member Create(config: FormatConfig, compareWithoutLineEndings: bool, file: string) : FormatParams =
        {
            Config = config
            CompareWithoutLineEndings = compareWithoutLineEndings
            File = file
        }

// Built once rather than inside `stripNewlines`, which runs twice for every file checked.
let carriageReturn: Text.RegularExpressions.Regex =
    Text.RegularExpressions.Regex(@"\r")

let formatContentAsync (formatParams: FormatParams) (originalContent: string) : Async<FormatResult> =
    async {
        try
            let isSignatureFile: bool = Path.GetExtension(formatParams.File) = ".fsi"

            let! { Code = formattedContent } =
                CodeFormatter.FormatDocumentAsync(isSignatureFile, originalContent, formatParams.Config)

            let contentChanged: bool =
                if formatParams.CompareWithoutLineEndings then
                    let stripNewlines (s: string) : string = carriageReturn.Replace(s, String.Empty)

                    (stripNewlines originalContent) <> (stripNewlines formattedContent)
                else
                    originalContent <> formattedContent

            if contentChanged then
                let! (isValid: bool) =
                    CodeFormatter.IsValidFSharpCodeAsync(isSignatureFile, formattedContent)

                if not isValid then
                    return FormatResult.InvalidCode(filename = formatParams.File, formattedContent = formattedContent)
                else
                    return FormatResult.Formatted(filename = formatParams.File, formattedContent = formattedContent)
            else
                return FormatResult.Unchanged(filename = formatParams.File)
        with ex ->
            return FormatResult.Error(formatParams.File, ex)
    }

/// A file as it was read: its text, and whether it began with a byte order mark. Both come out of
/// one read, because the mark is in the same bytes the text is decoded from.
type SourceFile =
    {
        Content: string
        HasByteOrderMark: bool
    }

/// Fantomas assumes the input files are UTF-8
/// As is stated in F# language spec: https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf#page=25
let readSourceFile (fs: IFileSystem) (file: string) : Async<SourceFile> =
    async {
        use stream = fs.File.OpenRead file

        use reader =
            new StreamReader(stream, UTF8Encoding(false), detectEncodingFromByteOrderMarks = true)

        let! (content: string) = reader.ReadToEndAsync() |> Async.AwaitTask

        // Opening the file again to look at its first three bytes would be needless: the reader has
        // already read them to settle on an encoding. A file that carried a mark comes back as
        // UTF-8 with that mark as its preamble, and one that did not keeps the encoding passed in,
        // whose preamble is empty. Checking the code page as well keeps this to the UTF-8 mark, as
        // reading the three bytes by hand did.
        let encoding: Encoding = reader.CurrentEncoding

        return
            {
                Content = content
                HasByteOrderMark = encoding.CodePage = Encoding.UTF8.CodePage && encoding.GetPreamble().Length > 0
            }
    }

/// Format content that is already in hand, settling there what `--force` means and what output
/// that is not valid F# comes to, so that a caller is left with one kind of failure to report.
let formatSource
    (env: CliEnvironment)
    (settings: CliSettings)
    (source: SourceFile)
    (file: string)
    (config: FormatConfig)
    : Async<FormatResult>
    =
    async {
        let formatParams: FormatParams = FormatParams.Create(config, false, file)

        let! (formatted: FormatResult) = formatContentAsync formatParams source.Content

        match formatted with
        | FormatResult.InvalidCode(f, formattedContent) when settings.Force ->
            // A warning, and on standard error, because it says Fantomas wrote F# it believes is not
            // valid. It used to go to standard out at Information, alongside the ordinary run of
            // things it is the opposite of.
            //
            // No status glyph, and it does not want one: the file is reported as formatted on the
            // line above, in the column every state shares. This is a note about that line rather
            // than a sixth state, and the two travel on different streams, so it repeats what
            // happened instead of leaning on a line a pipeline may not have beside it.
            let theme: Theme = env.ErrorTheme

            env.Log.Warning(
                String.Concat(
                    link theme f,
                    " was formatted, but the result is not valid F# code. It was written because ",
                    flagName theme "--force",
                    " was given."
                )
            )

            return FormatResult.Formatted(file, formattedContent)
        | FormatResult.InvalidCode(f, _) -> return FormatResult.Error(f, invalidResultException ())
        | FormatResult.Unchanged f as r ->
            env.Log.Debug $"'%s{f}' was unchanged"
            return r
        | r -> return r
    }

// The formatted text is held in memory and the output file is opened only once there is something
// to put in it. Opening it up front truncates it before the input is read, which empties the input
// when both paths turn out to name the same file, and leaves a zero byte file behind whenever
// formatting does not complete.
let processFile
    (env: CliEnvironment)
    (settings: CliSettings)
    (inputFile: string)
    (outputFile: string)
    : Async<FormatResult>
    =
    let fs: IFileSystem = env.FileSystem

    async {
        try
            env.Log.Debug $"Processing %s{inputFile}"
            let! (source: SourceFile) = readSourceFile fs inputFile
            let inPlace: bool = isSamePath fs inputFile outputFile

            let! (result: FormatResult) =
                formatSource env settings source inputFile (env.ReadConfiguration inputFile)

            let toWrite: string option =
                match result with
                | FormatResult.Formatted(_, formattedContent) -> Some formattedContent
                // Writing somewhere else has to carry an unchanged file across to it. Writing back
                // over the input has nothing to do.
                | FormatResult.Unchanged _ when not inPlace -> Some source.Content
                | _ -> None

            match toWrite with
            | None -> ()
            | Some contents ->
                if not inPlace then
                    ensureParentFolderExists fs outputFile

                if source.HasByteOrderMark then
                    do!
                        fs.File.WriteAllTextAsync(outputFile, contents, Encoding.UTF8)
                        |> Async.AwaitTask
                else
                    do! fs.File.WriteAllTextAsync(outputFile, contents) |> Async.AwaitTask

                env.Log.Debug $"%s{outputFile} has been written."

            return result
        with e ->
            return FormatResult.Error(inputFile, e)
    }

let runFormatCommand
    (env: CliEnvironment)
    (settings: CliSettings)
    (inputPath: InputPath)
    (outputPath: OutputPath)
    : FormatCommandResult
    =
    let fs: IFileSystem = env.FileSystem

    try
        // An output folder is created even when the run turns out to have nothing to put in it,
        // which is what a folder that is empty, or entirely ignored, comes to.
        match inputPath, outputPath with
        | InputPath.Folder _, OutputPath.IO outputFolder when not (fs.Directory.Exists outputFolder) ->
            fs.Directory.CreateDirectory outputFolder |> ignore
        | _ -> ()

        match plan fs env.Log env.FindIgnoreFile inputPath outputPath with
        | Error problem -> FormatCommandResult.InvalidInput problem
        | Ok items ->
            items
            |> List.map (fun item ->
                match item with
                | WorkItem.Ignored file -> async.Return(FormatResult.IgnoredFile file)
                | WorkItem.Format(inputFile, outputFile) -> processFile env settings inputFile outputFile
            )
            |> Async.Parallel
            |> Async.RunSynchronously
            |> FormatCommandResult.Completed
    with exn ->
        FormatCommandResult.Failed exn
