module Fantomas.FormatCommand

open System
open System.IO
open System.Text
open Fantomas.Core
open Fantomas
open Fantomas.Logging
open Fantomas.Arguments
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

    static member Create(compareWithoutLineEndings: bool, profile: bool, file: string) =
        { Config = EditorConfig.readConfiguration file
          CompareWithoutLineEndings = compareWithoutLineEndings
          Profile = profile
          File = file }

let private carriageReturn = Text.RegularExpressions.Regex(@"\r")

let formatContentAsync (formatParams: FormatParams) (originalContent: string) : Async<FormatResult> =
    async {
        try
            let isSignatureFile = Path.GetExtension(formatParams.File) = ".fsi"

            let! { Code = formattedContent }, profileInfo =
                if formatParams.Profile then
                    async {
                        let sw = Diagnostics.Stopwatch.StartNew()

                        let! res =
                            CodeFormatter.FormatDocumentAsync(isSignatureFile, originalContent, formatParams.Config)

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
                            CodeFormatter.FormatDocumentAsync(isSignatureFile, originalContent, formatParams.Config)

                        return res, None
                    }

            let contentChanged =
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

let hasByteOrderMark (file: string) : Async<bool> =
    async {
        if File.Exists(file) then
            let preamble = Encoding.UTF8.GetPreamble()

            use file = new FileStream(file, FileMode.Open, FileAccess.Read)

            let mutable bom = Array.zeroCreate 3
            do! file.ReadAsync(bom, 0, 3) |> Async.AwaitTask |> Async.Ignore<int>
            return bom = preamble
        else
            return false
    }

let processSourceString
    (force: bool)
    (profile: bool)
    (s: string)
    (fileName: string)
    (config: FormatConfig)
    : Async<FormatResult> =
    let writeResult (formatted: string) =
        async {
            let! hasBom = hasByteOrderMark fileName

            if hasBom then
                do! File.WriteAllTextAsync(fileName, formatted, Encoding.UTF8) |> Async.AwaitTask
            else
                do! File.WriteAllTextAsync(fileName, formatted) |> Async.AwaitTask

            logGrEqDetailed $"%s{fileName} has been written."
        }

    async {
        let formatParams = FormatParams.Create(config, false, profile, fileName)
        let! formatted = formatContentAsync formatParams s

        match formatted with
        | FormatResult.Formatted(_, formattedContent, _) as r ->
            do! formattedContent |> writeResult
            return r
        | FormatResult.InvalidCode(file, formattedContent) when force ->
            stdlog $"%s{file} was not valid after formatting."
            do! formattedContent |> writeResult
            return FormatResult.Formatted(fileName, formattedContent, None)
        | FormatResult.Unchanged(file, _) as r ->
            logGrEqDetailed $"'%s{file}' was unchanged"
            return r
        | FormatResult.IgnoredFile _ as r -> return r
        | FormatResult.Error _ as r -> return r
        | FormatResult.InvalidCode(file, _) -> return FormatResult.Error(file, invalidResultException file)
    }

let processSourceFile (force: bool) (profile: bool) (inFile: string) (tw: TextWriter) : Async<FormatResult> =
    async {
        let! originalContent = File.ReadAllTextAsync inFile |> Async.AwaitTask
        let! formatted = formatContentAsync (FormatParams.Create(false, profile, inFile)) originalContent

        match formatted with
        | FormatResult.Formatted(_, formattedContent, _) as r ->
            do! tw.WriteAsync(formattedContent) |> Async.AwaitTask
            return r
        | FormatResult.InvalidCode(file, formattedContent) when force ->
            stdlog $"%s{file} was not valid after formatting."
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
let fileToFile (force: bool) (profile: bool) (inFile: string) (outFile: string) : Async<FormatResult> =
    async {
        logGrEqDetailed $"Processing %s{inFile}"
        use buffer = new StringWriter()
        let! processResult = processSourceFile force profile inFile buffer

        match processResult with
        | FormatResult.Formatted _
        | FormatResult.Unchanged _ ->
            let! hasByteOrderMark = hasByteOrderMark inFile
            ensureParentFolderExists outFile
            let contents = buffer.ToString()

            if hasByteOrderMark then
                do! File.WriteAllTextAsync(outFile, contents, Encoding.UTF8) |> Async.AwaitTask
            else
                do! File.WriteAllTextAsync(outFile, contents) |> Async.AwaitTask

            logGrEqDetailed $"%s{outFile} has been written."
        | FormatResult.IgnoredFile _
        | FormatResult.InvalidCode _
        | FormatResult.Error _ -> ()

        return processResult
    }

let processFile (force: bool) (profile: bool) (inputFile: string) (outputFile: string) : Async<FormatResult> =
    async {
        try
            if not (isSamePath inputFile outputFile) then
                return! fileToFile force profile inputFile outputFile
            else
                logGrEqDetailed $"Processing %s{inputFile}"
                let! content = File.ReadAllTextAsync inputFile |> Async.AwaitTask
                let config = EditorConfig.readConfiguration inputFile
                return! processSourceString force profile content inputFile config
        with e ->
            return FormatResult.Error(inputFile, e)
    }

let runFormatCommand
    (force: bool)
    (profile: bool)
    (ignoreFile: IgnoreFile option)
    (inputPath: InputPath)
    (outputPath: OutputPath)
    : FormatCommandResult =
    try
        // An output folder is created even when the run turns out to have nothing to put in it,
        // which is what a folder that is empty, or entirely ignored, comes to.
        match inputPath, outputPath with
        | InputPath.Folder _, OutputPath.IO outputFolder when not (Directory.Exists outputFolder) ->
            Directory.CreateDirectory outputFolder |> ignore
        | _ -> ()

        match plan ignoreFile inputPath outputPath with
        | Error problem -> FormatCommandResult.InvalidInput problem
        | Ok items ->
            items
            |> List.map (fun item ->
                match item with
                | WorkItem.Ignored file -> async.Return(FormatResult.IgnoredFile file)
                | WorkItem.Format(inputFile, outputFile) -> processFile force profile inputFile outputFile)
            |> Async.Parallel
            |> Async.RunSynchronously
            |> FormatCommandResult.Completed
    with exn ->
        FormatCommandResult.Failed exn
