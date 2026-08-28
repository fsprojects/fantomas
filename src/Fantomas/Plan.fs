module Fantomas.Plan

open System
open System.Collections.Concurrent
open System.IO.Abstractions
open Fantomas
open Fantomas.Arguments
open Fantomas.CommandResult
open Serilog
open Fantomas.Paths

[<RequireQualifiedAccess>]
type WorkItem =
    | Ignored of file: string
    | Format of inputFile: string * outputFile: string

let plan
    (fs: IFileSystem)
    (log: ILogger)
    (findIgnoreFile: string -> IgnoreFile option)
    (inputPath: InputPath)
    (outputPath: OutputPath)
    : Result<WorkItem list, InputProblem>
    =
    let item (inputFile: string) (outputFile: string) : WorkItem =
        if IgnoreFile.isIgnoredFile log (findIgnoreFile inputFile) inputFile then
            WorkItem.Ignored inputFile
        else
            WorkItem.Format(inputFile, outputFile)

    // Asked once per ignore file rather than once per folder, because the answer is about the file
    // as a whole and the walk puts the question at every directory it meets.
    let negations: ConcurrentDictionary<string, bool> =
        ConcurrentDictionary<string, bool>()

    // A folder the ignore file names is never opened, so nothing inside it is planned, counted or
    // reported. `isIgnoredFile` reads a path rather than a file, so a directory is asked the same
    // question a file is; what makes this the parent's answer is that `findIgnoreFile` walks up
    // from the directory it is given, which for a folder is the one above it.
    //
    // Unless the ignore file negates something. A `!` line takes a path back out of what a line
    // above it matched, and the path it takes back out can be one inside a folder an earlier line
    // matched: `sub/*` followed by `!sub/keep` is how `.gitignore` spells "all of it but that
    // one". Closing `sub` would decide that `sub/keep` is not there and the line that takes it
    // back out would never be reached, so such an ignore file leaves every folder open and every
    // file inside is asked about one at a time.
    let isIgnoredDirectory (directory: string) : bool =
        match findIgnoreFile directory with
        | None -> false
        | Some ignoreFile ->

        let negates: bool =
            negations.GetOrAdd(
                ignoreFile.Location.FullName,
                Func<string, bool>(fun _ -> IgnoreFile.hasNegatedPattern ignoreFile)
            )

        if negates then
            false
        else

        let asDirectory: string =
            String.Concat(
                directory.TrimEnd(fs.Path.DirectorySeparatorChar),
                string<char> fs.Path.DirectorySeparatorChar
            )

        IgnoreFile.isIgnoredFile log (Some ignoreFile) asDirectory

    let folder (inputFolder: string) (outputFolder: string) : WorkItem list =
        let inPlace: bool = isSamePath fs inputFolder outputFolder

        // The output folder mirrors the input tree. Keeping only the file name would let two files
        // with the same name in different subfolders overwrite each other.
        let destinationOf (inputFile: string) : string =
            if inPlace then
                inputFile
            else
                fs.Path.Combine(outputFolder, fs.Path.GetRelativePath(inputFolder, inputFile))

        // An output folder inside the input folder is walked over as well, so the previous run's
        // results would be formatted again and nested one level deeper every time.
        let isPreviousOutput (inputFile: string) : bool =
            not inPlace && isInFolder fs outputFolder inputFile

        findAllFilesRecursively fs isIgnoredDirectory inputFolder
        |> Seq.choose (fun (found: Found) ->
            match found with
            // Said here, where it is known, and said only to whoever asked for detail. What is
            // inside such a folder is as unknown as what is inside a folder that is not there, so
            // there is nothing to add to it and nothing to count it towards. Reasoning about which
            // rules reach which path is a question worth answering properly one day, and a summary
            // line is not where it gets answered.
            | Found.IgnoredFolder folder ->
                log.Debug $"'%s{folder}' was not opened, .fantomasignore names it"
                None
            | Found.File i ->
                if isPreviousOutput i then
                    None
                else
                    Some(item i (destinationOf i))
        )
        |> Seq.toList

    match inputPath, outputPath with
    | InputPath.NoFSharpFile s, _ -> Error(InputProblem.UnsupportedFileType s)
    | InputPath.NotFound s, _ -> Error(InputProblem.NotFound s)
    | InputPath.Multiple _, OutputPath.IO _ -> Error InputProblem.MultiplePathsWithOut
    | InputPath.File p, OutputPath.NotKnown -> Ok [ item p p ]
    | InputPath.File p, OutputPath.IO o -> Ok [ item p o ]
    | InputPath.Folder p, OutputPath.NotKnown -> Ok(folder p p)
    | InputPath.Folder p, OutputPath.IO o -> Ok(folder p o)
    | InputPath.Multiple(files, folders), OutputPath.NotKnown ->
        Ok
            [
                yield! List.map (fun f -> item f f) files
                yield! List.collect (fun f -> folder f f) folders
            ]
