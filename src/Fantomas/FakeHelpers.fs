module Fantomas.FakeHelpers

open System
open System.IO
open FSharp.Compiler.SourceCodeServices
open Fantomas
open Fantomas.FormatConfig

// Share an F# checker instance across formatting calls
let sharedChecker = lazy(FSharpChecker.Create())

exception CodeFormatException of (string * Option<Exception>) array with
    override x.ToString() =
        let errors =
            x.Data0
            |> Array.choose (fun z ->
                match z with
                | file, Some ex -> Some(file, ex)
                | _ -> None)
            |> Array.map (fun z ->
                let file, ex = z
                file + ":\r\n" + ex.Message + "\r\n\r\n")

        let files =
            x.Data0
            |> Array.map (fun z ->
                match z with
                | file, Some _ -> file + " !"
                | file, None -> file)

        String.Join(String.Empty, errors)
        + "The following files aren't formatted properly:"
        + "\r\n- " + String.Join("\r\n- ", files)

type FormatResult =
    | Formatted of filename : string * formattedContent : string
    | Unchanged of filename : string
    | Error of filename : string * formattingError : Exception

let createParsingOptionsFromFile fileName =
    { FSharpParsingOptions.Default with SourceFiles = [|fileName|] }

let formatContentAsync config (file: string) (originalContent: string) =
    async {
        try
            let fileName =
                if Path.GetExtension(file) = ".fsi" then "tmp.fsi" else "tmp.fsx"

            let! formattedContent =
                CodeFormatter.FormatDocumentAsync(fileName, SourceOrigin.SourceString originalContent, config,
                                                  createParsingOptionsFromFile fileName ,sharedChecker.Value)

            if originalContent <> formattedContent then
                let! isValid =
                    CodeFormatter.IsValidFSharpCodeAsync(fileName, (SourceOrigin.SourceString(formattedContent)),
                                                         createParsingOptionsFromFile fileName, sharedChecker.Value)
                if not isValid  then
                    raise <| FormatException "Formatted content is not valid F# code"

                return Formatted(filename=file, formattedContent=formattedContent)
            else
                return Unchanged(filename=file)
        with
        | ex -> return Error(file, ex)
    }

let formatFileAsync (file : string) =
    let config = CodeFormatter.ReadConfiguration(file)
    let originalContent = File.ReadAllText file
    async {
        let! formatted = originalContent |> formatContentAsync config file
        return formatted
    }

let formatFilesAsync files =
    files
    |> Seq.map formatFileAsync
    |> Async.Parallel

let formatCode files =
    async {
        let! results = formatFilesAsync files
        
        // Check for formatting errors:
        let errors =
            results
            |> Array.choose (fun x ->
                match x with
                | Error(file, ex) -> Some(file, Some(ex))
                | _ -> None)

        if not <| Array.isEmpty errors then
            raise <| CodeFormatException errors

        // Overwritte source files with formatted content
        let result =
            results
            |> Array.choose (fun x ->
                match x with
                | Formatted(source, formatted) ->
                    File.WriteAllText(source, formatted)
                    Some source
                | _ -> None)

        return result
    }

type CheckResult =
    { Errors: (string * exn) list
      Formatted: string list }
    member this.HasErrors = List.isNotEmpty this.Errors
    member this.NeedsFormatting = List.isNotEmpty this.Formatted
    member this.IsValid = List.isEmpty this.Errors && List.isEmpty this.Formatted

/// Runs a check on the given files and reports the result to the given output:
///
/// * It shows the paths of the files that need formatting
/// * It shows the path and the error message of files that failed the format check
///
/// Returns:
///
/// A record with the file names that were formatted and the files that encounter problems while formatting.
let checkCode (filenames: seq<string>) =
    async {
        let! formatted = filenames
                         |> Seq.map formatFileAsync
                         |> Async.Parallel

        let getChangedFile =
            function
            | FormatResult.Unchanged(_) -> None
            | FormatResult.Formatted(f,_)
            | FormatResult.Error(f,_) -> Some f

        let changes =
            formatted
            |> Seq.choose getChangedFile
            |> Seq.toList

        let getErrors =
            function
            | FormatResult.Error(f,e) -> Some (f,e)
            | _ -> None

        let errors =
            formatted
            |> Seq.choose getErrors
            |> Seq.toList

        return { Errors = errors; Formatted = changes }
    }