module Fantomas.CheckCommand

open Fantomas
open Fantomas.Arguments
open Fantomas.CommandResult
open Fantomas.Paths

let runCheckCommand (inputPath: InputPath) : CheckCommandResult =
    let check (files: string seq) : CheckCommandResult =
        Async.RunSynchronously(Format.checkCode files) |> CheckCommandResult.Completed

    match inputPath with
    | InputPath.NoFSharpFile s -> CheckCommandResult.InvalidInput(InputProblem.UnsupportedFileType s)
    | InputPath.NotFound s -> CheckCommandResult.InvalidInput(InputProblem.NotFound s)
    | InputPath.Unspecified -> CheckCommandResult.InvalidInput InputProblem.NoPathGiven
    | InputPath.File f when (IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) f) ->
        CheckCommandResult.IgnoredFile f
    | InputPath.File path -> path |> Seq.singleton |> check
    | InputPath.Folder path -> path |> findAllFilesRecursively |> check
    | InputPath.Multiple(files, folders) ->
        seq {
            yield! files
            yield! (Seq.collect findAllFilesRecursively folders)
        }
        |> check
