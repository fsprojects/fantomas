module Fantomas.CheckCommand

open Fantomas.Arguments
open Fantomas.Cli
open Fantomas.CommandResult
open Fantomas.FormatCommand
open Fantomas.Plan

let checkCode (env: CliEnvironment) (filenames: string seq) : Async<CheckResult> =
    async {
        let! formatted =
            filenames
            |> Seq.map (fun filename ->
                async {
                    let! content = env.FileSystem.File.ReadAllTextAsync filename |> Async.AwaitTask

                    let formatParams: FormatParams =
                        FormatParams.Create(env.ReadConfiguration filename, true, false, filename)

                    return! formatContentAsync formatParams content
                })
            |> Async.Parallel

        let getChangedFile: FormatResult -> string option =
            function
            | FormatResult.Unchanged _
            | FormatResult.IgnoredFile _ -> None
            | FormatResult.Formatted(f, _, _)
            | FormatResult.Error(f, _)
            | FormatResult.InvalidCode(f, _) -> Some f

        let changes: string list = formatted |> Seq.choose getChangedFile |> Seq.toList

        let getErrors: FormatResult -> (string * exn) option =
            function
            | FormatResult.Error(f, e) -> Some(f, e)
            | _ -> None

        let errors: (string * exn) list = formatted |> Seq.choose getErrors |> Seq.toList

        return { Errors = errors; Formatted = changes }
    }

let runCheckCommand (env: CliEnvironment) (inputPath: InputPath) : CheckCommandResult =
    try
        // A check never writes, so the output path it plans against is the input itself.
        match plan env.FileSystem env.Log env.IgnoreFile inputPath OutputPath.NotKnown with
        | Error problem -> CheckCommandResult.InvalidInput problem
        | Ok items ->
            let ignored: string list =
                items
                |> List.choose (fun item ->
                    match item with
                    | WorkItem.Ignored file -> Some file
                    | WorkItem.Format _ -> None)

            let toCheck: string list =
                items
                |> List.choose (fun item ->
                    match item with
                    | WorkItem.Ignored _ -> None
                    | WorkItem.Format(inputFile, _) -> Some inputFile)

            CheckCommandResult.Completed(ignored, Async.RunSynchronously(checkCode env toCheck))
    with exn ->
        CheckCommandResult.Failed exn
