module Fantomas.CheckCommand

open System.IO
open Fantomas
open Fantomas.Arguments
open Fantomas.CommandResult
open Fantomas.FormatCommand
open Fantomas.Plan

let checkCode (filenames: string seq) : Async<CheckResult> =
    async {
        let! formatted =
            filenames
            |> Seq.map (fun filename ->
                async {
                    let! content = File.ReadAllTextAsync filename |> Async.AwaitTask
                    return! formatContentAsync (FormatParams.Create(true, false, filename)) content
                })
            |> Async.Parallel

        let getChangedFile =
            function
            | FormatResult.Unchanged _
            | FormatResult.IgnoredFile _ -> None
            | FormatResult.Formatted(f, _, _)
            | FormatResult.Error(f, _)
            | FormatResult.InvalidCode(f, _) -> Some f

        let changes = formatted |> Seq.choose getChangedFile |> Seq.toList

        let getErrors =
            function
            | FormatResult.Error(f, e) -> Some(f, e)
            | _ -> None

        let errors = formatted |> Seq.choose getErrors |> Seq.toList

        return { Errors = errors; Formatted = changes }
    }

let runCheckCommand (ignoreFile: IgnoreFile option) (inputPath: InputPath) : CheckCommandResult =
    // A check never writes, so the output path it plans against is the input itself.
    match plan ignoreFile inputPath OutputPath.NotKnown with
    | Error problem -> CheckCommandResult.InvalidInput problem
    | Ok items ->
        let ignored =
            items
            |> List.choose (fun item ->
                match item with
                | WorkItem.Ignored file -> Some file
                | WorkItem.Format _ -> None)

        let toCheck =
            items
            |> List.choose (fun item ->
                match item with
                | WorkItem.Ignored _ -> None
                | WorkItem.Format(inputFile, _) -> Some inputFile)

        CheckCommandResult.Completed(ignored, Async.RunSynchronously(checkCode toCheck))
