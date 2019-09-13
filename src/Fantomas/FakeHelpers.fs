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
    | Formatted of string * string
    | Unchanged of string
    | Error of string * Exception

let getParsingOptions fileName originalContent =
    let sourceOrigin = SourceOrigin.SourceString originalContent
    let sourceText,_ = CodeFormatterImpl.getSourceTextAndCode sourceOrigin
    let defines =
        TokenParser.getDefines originalContent
        |> List.map (sprintf "--define:%s")
        |> List.toArray
    sharedChecker.Value.GetProjectOptionsFromScript(fileName, sourceText, DateTime.Now, defines)
    |> (Async.RunSynchronously >> fst >> sharedChecker.Value.GetParsingOptionsFromProjectOptions >> fst)

let formatFileAsync config (file : string) =
    let originalContent = File.ReadAllText file

    async {
        try
            let parsingOptions = getParsingOptions file originalContent
            let! formattedContent = CodeFormatter.FormatDocumentAsync(file, SourceOrigin.SourceString originalContent, config, parsingOptions, sharedChecker.Value)
            if originalContent <> formattedContent then
                let! isValid = CodeFormatter.IsValidFSharpCodeAsync(file, (SourceOrigin.SourceString(formattedContent)), parsingOptions, sharedChecker.Value)
                if not isValid  then
                    raise <| FormatException "Formatted content is not valid F# code"

                let tempFile = Path.GetTempFileName()
                use output = new StreamWriter(tempFile)
                output.Write formattedContent
                output.Flush()

                return Formatted(file, tempFile)
            else
                return Unchanged file
        with
        | ex -> return Error(file, ex)
    }

let formatFilesAsync config files =
    files
    |> Seq.map (formatFileAsync config)
    |> Async.Parallel

let internal removeTemporaryFiles formatResult =
    match formatResult with
    | Formatted(_, tmp) -> File.Delete(tmp)
    | _ -> ()

let formatCode config files =
    async {
        let! results = files |> formatFilesAsync config
        
        try
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
                    | Formatted(source, tmp) ->
                        File.Copy(tmp, source, true)
                        Some source
                    | _ -> None)
                
            return result
        finally
            results |> Array.iter removeTemporaryFiles
    }


let checkCode config files =
    async {
        let! results = files |> formatFilesAsync config

        try
            let changes =
                results
                |> Array.choose (fun x ->
                    match x with
                    | Formatted(file, _) -> Some(file, None)
                    | Error(file, ex) -> Some(file, Some(ex))
                    | _ -> None)

            if Array.exists (function | _, Some(_) -> true | _ -> false) changes then
                raise <| CodeFormatException changes
        finally
            results |> Array.iter removeTemporaryFiles
    }