module Fantomas.InputFiles

open System.IO.Abstractions
open Fantomas.Logging

let acceptedFSharpExtensions = set [| ".fs"; ".fsx"; ".fsi"; ".ml"; ".mli" |]
let alwaysExcludedDirs = set [| "obj"; ".fable"; "fable_modules"; "node_modules" |]

let isFSharpFile (file: IFileInfo) =
    Set.contains file.Extension acceptedFSharpExtensions

let getFilesForFolder (fantomasIgnoreFile: IgnoreFile option) (directory: IDirectoryInfo) =
    let rec collectFileInFolder (directory: IDirectoryInfo) =
        seq {
            for file in directory.GetFiles() do
                if isFSharpFile file && not (IgnoreFile.isIgnoredFile fantomasIgnoreFile file) then
                    yield file

            for subDirectory in directory.GetDirectories() do
                if IgnoreFile.isIgnoredFolder fantomasIgnoreFile subDirectory then
                    logGrEqDetailed $"%s{subDirectory.FullName} is ignored via .fantomasignore file."
                else
                    yield! collectFileInFolder subDirectory
        }

    if not directory.Exists then
        logGrEqDetailed $"%s{directory.FullName} does not exist"
        Seq.empty
    elif alwaysExcludedDirs.Contains directory.Name then
        logGrEqDetailed $"%s{directory.FullName} is always ignored by Fantomas."
        Seq.empty
    else
        collectFileInFolder directory
