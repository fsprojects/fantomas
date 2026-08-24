module Fantomas.Plan

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
    (ignoreFile: IgnoreFile option)
    (inputPath: InputPath)
    (outputPath: OutputPath)
    : Result<WorkItem list, InputProblem>
    =
    let item (inputFile: string) (outputFile: string) : WorkItem =
        if IgnoreFile.isIgnoredFile log ignoreFile inputFile then
            WorkItem.Ignored inputFile
        else
            WorkItem.Format(inputFile, outputFile)

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

        findAllFilesRecursively fs inputFolder
        |> Seq.choose (fun i ->
            if isPreviousOutput i then
                None
            else
                Some(item i (destinationOf i))
        )
        |> Seq.toList

    match inputPath, outputPath with
    | InputPath.NoFSharpFile s, _ -> Error(InputProblem.UnsupportedFileType s)
    | InputPath.NotFound s, _ -> Error(InputProblem.NotFound s)
    | InputPath.Unspecified, _ -> Error InputProblem.NoPathGiven
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
