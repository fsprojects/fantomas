module Fantomas.FakeHelpers

open System
open System.IO

open Fantomas.CodeFormatter
open Fantomas.FormatConfig

exception CodeFormatException of (string * Option<Exception>) list with
    override x.ToString() =
        let errors =
            x.Data0
            |> List.choose (fun z ->
                match z with
                | file, Some ex -> Some(file, ex)
                | _ -> None)
            |> List.map (fun z ->
                let file, ex = z
                file + ":\r\n" + ex.Message + "\r\n\r\n")

        let files =
            x.Data0
            |> List.map (fun z ->
                match z with
                | file, Some _ -> file + " !"
                | file, None -> file)

        String.Join(String.Empty, errors)
        + "The following files aren't formatted properly:"
        + "\r\n- " + String.Join("\r\n- ", files)

type internal FormatResult =
    | Formatted of string * string
    | Unchanged of string
    | Error of string * Exception

let internal formatFile config (file : string) =
    let isSignatureFile = file.EndsWith(".fsi") || file.EndsWith(".mli")
    let originalContent = File.ReadAllText file

    try
        let formattedContent = CodeFormatter.formatSourceString isSignatureFile originalContent config
        if originalContent <> formattedContent then
            if not <| isValidFSharpCode isSignatureFile formattedContent then
                raise <| FormatException "Formatted content is not valid F# code"

            let tempFile = Path.GetTempFileName()
            use output = new StreamWriter(tempFile)
            output.Write formattedContent
            output.Flush()

            Formatted(file, tempFile)
        else Unchanged file
    with
    | ex -> Error(file, ex)

let internal formatFiles config files =
    files
    |> Seq.toList
    |> List.map (formatFile config)

let internal removeTemporaryFiles formatResult =
    match formatResult with
    | Formatted(_, tmp) -> File.Delete(tmp)
    | _ -> ()

let formatCode config files =
    let results = files |> formatFiles config

    try
        // Check for formatting errors:
        let errors =
            results
            |> List.choose (fun x ->
                match x with
                | Error(file, ex) -> Some(file, Some(ex))
                | _ -> None)

        if not <| List.isEmpty errors then
            raise <| CodeFormatException errors

        // Overwritte source files with formatted content
        results
        |> List.choose (fun x ->
            match x with
            | Formatted(source, tmp) ->
                File.Copy(tmp, source, true)
                Some source
            | _ -> None)
    finally
        results |> List.iter removeTemporaryFiles

let checkCode config files =
    let results = files |> formatFiles config

    try
        let changes =
            results
            |> List.choose (fun x ->
                match x with
                | Formatted(file, _) -> Some(file, None)
                | Error(file, ex) -> Some(file, Some(ex))
                | _ -> None)

        if not <| List.isEmpty changes then
            raise <| CodeFormatException changes
    finally
        results |> List.iter removeTemporaryFiles
