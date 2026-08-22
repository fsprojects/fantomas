module Fantomas.Plan

open System.IO
open Fantomas
open Fantomas.Arguments
open Fantomas.CommandResult
open Fantomas.Paths

[<RequireQualifiedAccess>]
type WorkItem =
    | Ignored of file: string
    | Format of inputFile: string * outputFile: string

let plan
    (ignoreFile: IgnoreFile option)
    (inputPath: InputPath)
    (outputPath: OutputPath)
    : Result<WorkItem list, InputProblem> =
    let item (inputFile: string) (outputFile: string) : WorkItem =
        if IgnoreFile.isIgnoredFile ignoreFile inputFile then
            WorkItem.Ignored inputFile
        else
            WorkItem.Format(inputFile, outputFile)

    let folder (inputFolder: string) (outputFolder: string) : WorkItem list =
        let inPlace = isSamePath inputFolder outputFolder

        findAllFilesRecursively inputFolder
        // An output folder inside the input folder is walked over as well, so the previous run's
        // results would be formatted again and nested one level deeper every time.
        |> Seq.filter (fun i -> inPlace || not (isInFolder outputFolder i))
        |> Seq.map (fun i ->
            let o =
                if inPlace then
                    i
                else
                    // The output folder mirrors the input tree. Keeping only the file name would
                    // let two files with the same name in different subfolders overwrite each other.
                    Path.Combine(outputFolder, Path.GetRelativePath(inputFolder, i))

            item i o)
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
            [ yield! List.map (fun f -> item f f) files
              yield! List.collect (fun f -> folder f f) folders ]
