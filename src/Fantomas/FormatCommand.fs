module Fantomas.FormatCommand

open System.IO
open System.Text
// Fantomas.Core has a FormatResult of its own. Opening Fantomas last is what makes the
// FormatResult named here the one this project defines.
open Fantomas.Core
open Fantomas
open Fantomas.Logging
open Fantomas.Arguments
open Fantomas.CommandResult
open Fantomas.Paths

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

let processSourceString (force: bool) (profile: bool) s (fileName: string) config =
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
        let! formatted = s |> Format.formatContentAsync formatParams

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
        | FormatResult.IgnoredFile file as r ->
            logGrEqDetailed $"'%s{file}' was ignored"
            return r
        | FormatResult.Error _ as r -> return r
        | FormatResult.InvalidCode(file, _) ->
            let ex = Format.invalidResultException file
            return FormatResult.Error(file, ex)
    }

let processSourceFile (force: bool) (profile: bool) inFile (tw: TextWriter) =
    async {
        let! formatted = FormatParams.Create(false, profile, inFile) |> Format.formatFileAsync

        match formatted with
        | FormatResult.Formatted(_, formattedContent, _) as r ->
            do! tw.WriteAsync(formattedContent) |> Async.AwaitTask
            return r
        | FormatResult.InvalidCode(file, formattedContent) when force ->
            stdlog $"%s{file} was not valid after formatting."
            do! tw.WriteAsync(formattedContent) |> Async.AwaitTask
            return FormatResult.Formatted(inFile, formattedContent, None)
        | FormatResult.Unchanged _ as r ->
            let! input = inFile |> File.ReadAllTextAsync |> Async.AwaitTask
            do! input |> tw.WriteAsync |> Async.AwaitTask
            return r
        | FormatResult.IgnoredFile file as r ->
            logGrEqDetailed $"'%s{file}' was ignored"
            return r
        | FormatResult.Error _ as r -> return r
        | FormatResult.InvalidCode(file, _) ->
            let ex = Format.invalidResultException file
            return FormatResult.Error(file, ex)
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

let processFolder
    (force: bool)
    (profile: bool)
    (inputFolder: string)
    (outputFolder: string)
    : Async<FormatResult> list =
    if not <| Directory.Exists(outputFolder) then
        Directory.CreateDirectory(outputFolder) |> ignore

    let inPlace = isSamePath inputFolder outputFolder

    findAllFilesRecursively inputFolder
    // An output folder inside the input folder is walked over as well, so the previous run's
    // results would be formatted again and nested one level deeper every time.
    |> Seq.filter (fun i -> inPlace || not (isInFolder outputFolder i))
    |> Seq.toList
    |> List.map (fun i ->

        let o =
            if inPlace then
                i
            else
                // The output folder mirrors the input tree. Keeping only the file name would
                // let two files with the same name in different subfolders overwrite each other.
                // fileToFile creates the folders leading up to the file.
                Path.Combine(outputFolder, Path.GetRelativePath(inputFolder, i))

        processFile force profile i o)

let filesAndFolders
    (force: bool)
    (profile: bool)
    (files: string list)
    (folders: string list)
    : Async<FormatResult> list =
    let fileTasks =
        files
        |> List.map (fun file ->
            if (IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) file) then
                logGrEqDetailed $"'%s{file}' was ignored"
                async.Return(FormatResult.IgnoredFile(file))
            else
                processFile force profile file file)

    let folderTasks =
        folders
        |> List.collect (fun folder -> processFolder force profile folder folder)

    (fileTasks @ folderTasks)

let asyncRunner (computations: Async<FormatResult> list) : FormatResult array =
    computations |> Async.Parallel |> Async.RunSynchronously

let runFormatCommand
    (force: bool)
    (profile: bool)
    (inputPath: InputPath)
    (outputPath: OutputPath)
    : FormatCommandResult =
    try
        match inputPath, outputPath with
        | InputPath.NoFSharpFile s, _ -> FormatCommandResult.InvalidInput(InputProblem.UnsupportedFileType s)
        | InputPath.NotFound s, _ -> FormatCommandResult.InvalidInput(InputProblem.NotFound s)
        | InputPath.Unspecified, _ -> FormatCommandResult.InvalidInput InputProblem.NoPathGiven
        | InputPath.File f, _ when (IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) f) ->
            FormatCommandResult.IgnoredFile f
        | InputPath.Folder p1, OutputPath.NotKnown ->
            processFolder force profile p1 p1
            |> asyncRunner
            |> FormatCommandResult.Completed
        | InputPath.File p1, OutputPath.NotKnown ->
            processFile force profile p1 p1
            |> List.singleton
            |> asyncRunner
            |> FormatCommandResult.Completed
        | InputPath.File p1, OutputPath.IO p2 ->
            processFile force profile p1 p2
            |> List.singleton
            |> asyncRunner
            |> FormatCommandResult.Completed
        | InputPath.Folder p1, OutputPath.IO p2 ->
            processFolder force profile p1 p2
            |> asyncRunner
            |> FormatCommandResult.Completed
        | InputPath.Multiple(files, folders), OutputPath.NotKnown ->
            filesAndFolders force profile files folders
            |> asyncRunner
            |> FormatCommandResult.Completed
        | InputPath.Multiple _, OutputPath.IO _ -> FormatCommandResult.InvalidInput InputProblem.MultiplePathsWithOut
    with exn ->
        FormatCommandResult.Failed exn
