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
                    let! (content: string) =
                        env.FileSystem.File.ReadAllTextAsync filename |> Async.AwaitTask

                    let formatParams: FormatParams =
                        FormatParams.Create(env.ReadConfiguration filename, true, filename)

                    return! formatContentAsync formatParams content
                }
            )
            |> Async.Parallel

        // A file that could not be parsed is an error and nothing else. Counting it as needing
        // formatting too reported the same file twice, once under each heading, and told the
        // reader to run a formatter that had already failed on it.
        let getChangedFile: FormatResult -> string option =
            function
            | FormatResult.Formatted(f, _) -> Some f
            | FormatResult.Unchanged _
            | FormatResult.IgnoredFile _
            | FormatResult.Error _
            | FormatResult.InvalidCode _ -> None

        let changes: string list = formatted |> Seq.choose getChangedFile |> Seq.toList

        // InvalidCode is a failure of Fantomas rather than of the file, and it was previously
        // reported as needing formatting, which named neither.
        let getErrors: FormatResult -> (string * exn) option =
            function
            | FormatResult.Error(f, e) -> Some(f, e)
            | FormatResult.InvalidCode(f, _) -> Some(f, invalidResultException () :> exn)
            | _ -> None

        let errors: (string * exn) list = formatted |> Seq.choose getErrors |> Seq.toList

        // A file the check found nothing to say about. Nothing branches on these, but a report that
        // names every file the run looked at needs them, and only this knows which they were.
        let getUnchangedFile: FormatResult -> string option =
            function
            | FormatResult.Unchanged f -> Some f
            | FormatResult.IgnoredFile _
            | FormatResult.Formatted _
            | FormatResult.Error _
            | FormatResult.InvalidCode _ -> None

        let unchanged: string list = formatted |> Seq.choose getUnchangedFile |> Seq.toList

        return
            {
                Errors = errors
                Formatted = changes
                Unchanged = unchanged
            }
    }

let runCheckCommand (env: CliEnvironment) (inputPath: InputPath) : CheckCommandResult =
    try
        // A check never writes, so the output path it plans against is the input itself.
        match plan env.FileSystem env.Log env.FindIgnoreFile inputPath OutputPath.NotKnown with
        | Error problem -> CheckCommandResult.InvalidInput problem
        | Ok items ->
            let ignored: string list =
                items
                |> List.choose (fun item ->
                    match item with
                    | WorkItem.Ignored file -> Some file
                    | WorkItem.Format _ -> None
                )

            let toCheck: string list =
                items
                |> List.choose (fun item ->
                    match item with
                    | WorkItem.Ignored _ -> None
                    | WorkItem.Format(inputFile, _) -> Some inputFile
                )

            CheckCommandResult.Completed(ignored, Async.RunSynchronously(checkCode env toCheck))
    with exn ->
        CheckCommandResult.Failed exn
