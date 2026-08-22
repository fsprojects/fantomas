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

let private carriageReturn = Text.RegularExpressions.Regex(@"\r")

let formatContentAsync (formatParams: FormatParams) (originalContent: string) : Async<FormatResult> =
    async {
        try
            let isSignatureFile: bool = Path.GetExtension(formatParams.File) = ".fsi"

            let! { Code = formattedContent }, profileInfo =
                if formatParams.Profile then
                    async {
                        let sw: Diagnostics.Stopwatch = Diagnostics.Stopwatch.StartNew()

                        let! res =
                            CodeFormatter.FormatDocumentAsync(isSignatureFile, originalContent, formatParams.Config)

                        sw.Stop()

                        let count: int =
                            originalContent.Length - originalContent.Replace(Environment.NewLine, "").Length

                        let profileInfo: ProfileInfo =
                            { LineCount = count
                              TimeTaken = sw.Elapsed }

                        return res, Some profileInfo
                    }
                else
                    async {
                        let! res =
                            CodeFormatter.FormatDocumentAsync(isSignatureFile, originalContent, formatParams.Config)

                        return res, None
                    }

            let contentChanged: bool =
                if formatParams.CompareWithoutLineEndings then
                    let stripNewlines (s: string) = carriageReturn.Replace(s, String.Empty)

                    (stripNewlines originalContent) <> (stripNewlines formattedContent)
                else
                    originalContent <> formattedContent

            if contentChanged then
                let! isValid = CodeFormatter.IsValidFSharpCodeAsync(isSignatureFile, formattedContent)

                if not isValid then
                    return FormatResult.InvalidCode(filename = formatParams.File, formattedContent = formattedContent)
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

let hasByteOrderMark (fs: IFileSystem) (file: string) : Async<bool> =
    async {
        if fs.File.Exists(file) then
            let preamble: byte array = Encoding.UTF8.GetPreamble()

            use stream = fs.File.OpenRead(file)

            let mutable bom: byte array = Array.zeroCreate 3
            do! stream.ReadAsync(bom, 0, 3) |> Async.AwaitTask |> Async.Ignore<int>
            return bom = preamble
        else
            return false
    }

let processSourceString
    (env: CliEnvironment)
    (settings: CliSettings)
    (s: string)
    (fileName: string)
    (config: FormatConfig)
    : Async<FormatResult> =
    let fs: IFileSystem = env.FileSystem
    let force: bool = settings.Force

    let writeResult (formatted: string) =
        async {
            let! hasBom = hasByteOrderMark fs fileName

            if hasBom then
                do! fs.File.WriteAllTextAsync(fileName, formatted, Encoding.UTF8) |> Async.AwaitTask
            else
                do! fs.File.WriteAllTextAsync(fileName, formatted) |> Async.AwaitTask

            env.Log.Debug $"%s{fileName} has been written."
        }

    async {
        let formatParams: FormatParams =
            FormatParams.Create(config, false, settings.Profile, fileName)

        let! formatted = formatContentAsync formatParams s

        match formatted with
        | FormatResult.Formatted(_, formattedContent, _) as r ->
            do! formattedContent |> writeResult
            return r
        | FormatResult.InvalidCode(file, formattedContent) when force ->
            env.Log.Information $"%s{file} was not valid after formatting."
            do! formattedContent |> writeResult
            return FormatResult.Formatted(fileName, formattedContent, None)
        | FormatResult.Unchanged(file, _) as r ->
            env.Log.Debug $"'%s{file}' was unchanged"
            return r
        | FormatResult.IgnoredFile _ as r -> return r
        | FormatResult.Error _ as r -> return r
        | FormatResult.InvalidCode(file, _) -> return FormatResult.Error(file, invalidResultException file)
    }

let processSourceFile
    (env: CliEnvironment)
    (settings: CliSettings)
    (inFile: string)
    (tw: TextWriter)
    : Async<FormatResult> =
    let force: bool = settings.Force

    async {
        let! originalContent = env.FileSystem.File.ReadAllTextAsync inFile |> Async.AwaitTask

        let formatParams: FormatParams =
            FormatParams.Create(env.ReadConfiguration inFile, false, settings.Profile, inFile)

        let! formatted = formatContentAsync formatParams originalContent

        match formatted with
        | FormatResult.Formatted(_, formattedContent, _) as r ->
            do! tw.WriteAsync(formattedContent) |> Async.AwaitTask
            return r
        | FormatResult.InvalidCode(file, formattedContent) when force ->
            env.Log.Information $"%s{file} was not valid after formatting."
            do! tw.WriteAsync(formattedContent) |> Async.AwaitTask
            return FormatResult.Formatted(inFile, formattedContent, None)
        | FormatResult.Unchanged _ as r ->
            // The content is already in hand, so an unchanged file is copied across from it
            // rather than read a second time.
            do! originalContent |> tw.WriteAsync |> Async.AwaitTask
            return r
        | FormatResult.IgnoredFile _ as r -> return r
        | FormatResult.Error _ as r -> return r
        | FormatResult.InvalidCode(file, _) -> return FormatResult.Error(file, invalidResultException file)
    }

// The formatted text is collected in memory and the output file is opened only once there
// is something to put in it. Opening it up front truncates it before the input is read,
// which empties the input when both paths turn out to name the same file, and leaves a
// zero byte file behind whenever formatting does not complete.
let fileToFile (env: CliEnvironment) (settings: CliSettings) (inFile: string) (outFile: string) : Async<FormatResult> =
    let fs: IFileSystem = env.FileSystem

    async {
        env.Log.Debug $"Processing %s{inFile}"
        use buffer = new StringWriter()
        let! processResult = processSourceFile env settings inFile buffer

        match processResult with
        | FormatResult.Formatted _
        | FormatResult.Unchanged _ ->
            let! hasByteOrderMark = hasByteOrderMark fs inFile
            ensureParentFolderExists fs outFile
            let contents: string = buffer.ToString()

            if hasByteOrderMark then
                do! fs.File.WriteAllTextAsync(outFile, contents, Encoding.UTF8) |> Async.AwaitTask
            else
                do! fs.File.WriteAllTextAsync(outFile, contents) |> Async.AwaitTask

            env.Log.Debug $"%s{outFile} has been written."
        | FormatResult.IgnoredFile _
        | FormatResult.InvalidCode _
        | FormatResult.Error _ -> ()

        return processResult
    }

let processFile
    (env: CliEnvironment)
    (settings: CliSettings)
    (inputFile: string)
    (outputFile: string)
    : Async<FormatResult> =
    async {
        try
            if not (isSamePath env.FileSystem inputFile outputFile) then
                return! fileToFile env settings inputFile outputFile
            else
                env.Log.Debug $"Processing %s{inputFile}"
                let! content = env.FileSystem.File.ReadAllTextAsync inputFile |> Async.AwaitTask
                return! processSourceString env settings content inputFile (env.ReadConfiguration inputFile)
        with e ->
            return FormatResult.Error(inputFile, e)
    }

let runFormatCommand
    (env: CliEnvironment)
    (settings: CliSettings)
    (inputPath: InputPath)
    (outputPath: OutputPath)
    : FormatCommandResult =
    let fs: IFileSystem = env.FileSystem

    try
        // An output folder is created even when the run turns out to have nothing to put in it,
        // which is what a folder that is empty, or entirely ignored, comes to.
        match inputPath, outputPath with
        | InputPath.Folder _, OutputPath.IO outputFolder when not (fs.Directory.Exists outputFolder) ->
            fs.Directory.CreateDirectory outputFolder |> ignore
        | _ -> ()

        match plan fs env.Log env.IgnoreFile inputPath outputPath with
        | Error problem -> FormatCommandResult.InvalidInput problem
        | Ok items ->
            items
            |> List.map (fun item ->
                match item with
                | WorkItem.Ignored file -> async.Return(FormatResult.IgnoredFile file)
                | WorkItem.Format(inputFile, outputFile) -> processFile env settings inputFile outputFile)
            |> Async.Parallel
            |> Async.RunSynchronously
            |> FormatCommandResult.Completed
    with exn ->
        FormatCommandResult.Failed exn
