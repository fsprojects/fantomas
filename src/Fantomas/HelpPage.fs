module Fantomas.HelpPage

open System
open Argu
open Fantomas.Core
open Fantomas.Theme

// The column the right hand half of a two column row starts in.
let descriptionColumn: int = 29
let exampleColumn: int = 33
let linkColumn: int = 29

// Trim the commit hash the version carries down to the short form git itself shows.
let shortVersion () : string =
    let version: string = CodeFormatter.GetVersion()

    match version.Split('+') with
    | [| number; commit |] when commit.Length > 9 -> String.Concat(number, "+", commit.Substring(0, 9))
    | _ -> version

let flags: (string * string * string * string list) list =
    [
        ("",
         "--check",
         "",
         [
             "Report which files need formatting and write nothing."
             "Exits 0 when every file is already formatted, 99 when some"
             "file needs formatting, and 1 when an error occurred."
         ])
        ("",
         "--out",
         "<path>",
         [
             "Write the result to this file or folder instead of formatting"
             "in place. Takes a single input path."
         ])
        ("",
         "--force",
         "",
         [
             "Write the output even when it is not valid F# code."
             "For debugging purposes only."
         ])
        ("", "--profile", "", [ "Print the line count and the time taken for every file." ])
        ("",
         "--json",
         "",
         [
             "Report what the run did as one JSON document on standard out,"
             "naming every file and positioning what went wrong. The usual"
             "messages are not printed; warnings go to standard error."
         ])
        ("",
         "--daemon",
         "",
         [
             "Run an LSP-like server that editor tooling can talk to."
             "Takes no other flags or paths, apart from --verbosity."
         ])
        ("-v",
         "--verbosity",
         "<level>",
         [
             "How much to print: normal or detailed. Defaults to normal."
             "n and d are accepted as well."
         ])
        ("", "--version", "", [ "Print the version and exit" ])
        ("-h", "--help", "", [ "Display this menu and exit" ])
    ]

let examples: (string * string) list =
    [
        ("fantomas .", "Format every F# file below the current folder")
        ("fantomas src/App.fs", "Format a single file in place")
        ("fantomas --check .", "Report what needs formatting, write nothing")
        ("fantomas --out build src", "Copy the formatted files to another folder")
    ]

let links: (string * string list) list =
    [
        ("Learn more about Fantomas:", [ "https://fsprojects.github.io/fantomas/docs" ])
        ("Configure Fantomas:", [ "https://fsprojects.github.io/fantomas/docs/end-users/Configuration.html" ])
        ("Join the F# Discord:", [ "https://discord.com/channels/196693847965696000/1493226271767924747" ])
        ("Docs for your LLM:",
         [
             "https://fsprojects.github.io/fantomas/llms.txt"
             "https://fsprojects.github.io/fantomas/llms-full.txt"
         ])
    ]

let writeFlag
    (write: string -> unit)
    (theme: Theme)
    (short: string, long: string, argument: string, description: string list)
    : unit
    =
    let shortPart: string =
        if String.IsNullOrEmpty short then
            "    "
        else
            String.Concat(flagName theme short, ", ")

    let argumentPart: string =
        if String.IsNullOrEmpty argument then
            ""
        else
            String.Concat(" ", placeholder theme argument)

    let left: string = String.Concat("  ", shortPart, flagName theme long, argumentPart)

    match description with
    | [] -> write left
    | first :: rest ->
        writeRow write descriptionColumn left first
        List.iter (writeContinuation write descriptionColumn) rest

let writeExample (write: string -> unit) (theme: Theme) ((command, description): string * string) : unit =
    let name, arguments: string * string =
        match command.IndexOf ' ' with
        | -1 -> command, ""
        | i -> command.Substring(0, i), command.Substring(i)

    writeRow write exampleColumn (String.Concat("  ", muted theme name, flagName theme arguments)) description

let writeLink (write: string -> unit) (theme: Theme) ((label, urls): string * string list) : unit =
    match urls with
    | [] -> ()
    | first :: rest ->
        writeRow write linkColumn label (link theme first)
        List.iter (fun (url: string) -> writeContinuation write linkColumn (link theme url)) rest

let render (theme: Theme) : string list =
    let lines: ResizeArray<string> = ResizeArray()
    let write (line: string) : unit = lines.Add line
    let blank () : unit = lines.Add ""

    write (
        String.Concat(
            title theme "Fantomas",
            " is an opinionated source code formatter for F#. ",
            muted theme (String.Concat("(", shortVersion (), ")"))
        )
    )

    blank ()

    write (
        String.Concat(
            heading theme "Usage:",
            " ",
            heading theme "fantomas",
            " ",
            flagName theme "[...flags] [...paths]"
        )
    )

    blank ()
    write (heading theme "Examples:")
    List.iter (writeExample write theme) examples
    blank ()
    write (heading theme "Flags:")
    List.iter (writeFlag write theme) flags
    blank ()
    write (heading theme "Paths:")
    write "  A path is a folder, which is searched recursively, or a file ending in .fs, .fsi,"
    write "  .fsx, .ml or .mli. Formatting settings are read from .editorconfig, and files"
    write "  matched by .fantomasignore in the current folder are skipped."
    blank ()
    List.iter (writeLink write theme) links
    blank ()
    List.ofSeq lines

let print () : unit =
    render (forOutput ()) |> List.iter Console.Out.WriteLine

// Argu builds a usage block of its own and hands it over as part of the message. Only the
// first line carries the actual complaint, so the rest is dropped in favour of a pointer to
// the page above.
let complaint (message: string) : string =
    message.Split('\n')
    |> Array.tryFind (fun (line: string) -> line.StartsWith("ERROR:", StringComparison.Ordinal))
    |> Option.defaultValue message
    |> fun (line: string) -> line.Trim()

let exiter: IExiter =
    { new IExiter with
        member _.Name = "Fantomas help page"

        member _.Exit(message: string, errorCode: ErrorCode) =
            if errorCode = ErrorCode.HelpText then
                print ()
                exit 0
            else
                Console.Error.WriteLine(complaint message)
                Console.Error.WriteLine("Run fantomas --help for usage information.")
                exit (int errorCode)
    }
