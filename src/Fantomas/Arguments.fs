module Fantomas.Arguments

open System.IO.Abstractions
open Argu
open Fantomas.Logging
open Fantomas.Paths

// The Argu attributes live in the signature file. Repeating them here compiles only the
// signature's copy and warns about the two whose arguments the compiler cannot match up.
type Arguments =
    | Force
    | Profile
    | Out of string
    | Check
    | Daemon
    | Version
    | Verbosity of string
    | Input of string list

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Force -> "Print the output even if it is not valid F# code. For debugging purposes only."
            | Out _ ->
                "Give a valid path for files/folders. Files should have .fs, .fsx, .fsi, .ml or .mli extension only. Multiple files/folders are not supported."
            | Profile -> "Print performance profiling information."
            | Check ->
                "Report which files need formatting and write nothing. Exits with 0 when every file is already formatted, with 99 when some file needs formatting, and with 1 when an error occurred."
            | Daemon -> "Daemon mode, launches an LSP-like server that can be used by editor tooling."
            | Version -> "Displays the version of Fantomas"
            | Input _ ->
                sprintf
                    "Input paths: can be multiple folders or files with %s extension."
                    (Seq.map (fun s -> "*" + s) extensions |> String.concat ",")
            | Verbosity _ -> "Set the verbosity level. Allowed values are n[ormal] and d[etailed]."

[<RequireQualifiedAccess>]
type InputPath =
    | File of string
    | Folder of string
    | Multiple of files: string list * folder: string list
    | NoFSharpFile of string
    | NotFound of string
    | Unspecified

[<RequireQualifiedAccess; Struct>]
type OutputPath =
    | IO of string
    | NotKnown

let classifyInputPath (fs: IFileSystem) (maybeInput: string list option) : InputPath =
    match maybeInput with
    | Some [ input ] ->
        if fs.Directory.Exists(input) then
            InputPath.Folder input
        elif fs.File.Exists input && isFSharpFile input then
            InputPath.File input
        elif fs.File.Exists input then
            InputPath.NoFSharpFile input
        else
            InputPath.NotFound input
    | Some inputs ->
        let missing: string option =
            inputs
            |> List.tryFind (fun x -> not (fs.Directory.Exists(x) || fs.File.Exists(x)))

        match missing with
        | Some x -> InputPath.NotFound x
        | None ->
            // Every path here is known to exist, so which of the two it is can be asked rather
            // than guessed from whether it carries an extension. Guessing called a folder named
            // `my.stuff` a file, and a file with no extension a folder.
            let folders, files =
                inputs |> List.partition (fun (path: string) -> fs.Directory.Exists path)

            InputPath.Multiple(files, folders)
    | None -> InputPath.Unspecified

let parseVerbosity (value: string option) : VerbosityLevel option =
    match value |> Option.map (fun v -> v.ToLowerInvariant()) with
    | None
    | Some "n"
    | Some "normal" -> Some VerbosityLevel.Normal
    | Some "d"
    | Some "detailed" -> Some VerbosityLevel.Detailed
    | Some _ -> None
