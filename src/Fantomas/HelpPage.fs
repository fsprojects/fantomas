module Fantomas.HelpPage

open System
open System.Text.RegularExpressions
open Argu
open Spectre.Console
open Fantomas.Core

// Select graphic rendition sequences, so a decorated string can still be measured.
let escapeSequence = Regex(@"\u001b\[[0-9;]*m", RegexOptions.Compiled)

// What the terminal can do is Spectre.Console's answer to give: it knows the TERM values, it
// honours NO_COLOR, and it reports which colour system is available.
let capabilities = lazy AnsiConsole.Profile.Capabilities

// Redirection is decided here rather than left to Spectre. Spectre turns ANSI back on when it
// detects a CI environment, because a CI log viewer renders escape codes and a progress bar
// there is worth colouring. A help page is not: it gets piped into a file, a pager or a script
// that reads it, on a build agent as much as anywhere else. Standard out being a terminal is
// the question this page needs answered, so both have to agree.
let colorsEnabled =
    lazy
        (not Console.IsOutputRedirected
         && capabilities.Value.Ansi
         && capabilities.Value.ColorSystem <> ColorSystem.NoColors)

let eightBitColors = lazy (capabilities.Value.ColorSystem >= ColorSystem.EightBit)

// The palette is written out as escape codes rather than drawn by Spectre, because Spectre
// wraps what it writes to the console width and this page is laid out in fixed columns.
let decorate (eightBit: string) (fallback: string) (text: string) =
    if not colorsEnabled.Value then
        text
    else
        let code = if eightBitColors.Value then eightBit else fallback
        String.Concat("\u001b[", code, "m", text, "\u001b[0m")

// 38;5;38 is the closest 256 colour to the blue the website uses.
let title text = decorate "1;38;5;38" "1;36" text
let link text = decorate "38;5;38" "36" text
let heading text = decorate "1" "1" text
let flagName text = decorate "1;38;5;80" "1;36" text
let placeholder text = decorate "38;5;245" "2" text
let muted text = decorate "2" "2" text

// The column the right hand half of a two column row starts in.
let descriptionColumn = 29
let exampleColumn = 33
let linkColumn = 29

let write (line: string) = Console.Out.WriteLine(line)
let blank () = Console.Out.WriteLine()

let visibleLength (text: string) = escapeSequence.Replace(text, "").Length

let writeRow (column: int) (left: string) (right: string) =
    let padding = String(' ', max 1 (column - visibleLength left))
    write (String.Concat(left, padding, right))

let writeContinuation (column: int) (right: string) =
    write (String.Concat(String(' ', column), right))

// Trim the commit hash the version carries down to the short form git itself shows.
let shortVersion () =
    let version = CodeFormatter.GetVersion()

    match version.Split('+') with
    | [| number; commit |] when commit.Length > 9 -> String.Concat(number, "+", commit.Substring(0, 9))
    | _ -> version

let flags =
    [ ("",
       "--check",
       "",
       [ "Report which files need formatting and write nothing."
         "Exits 0 when every file is already formatted, 99 when some"
         "file needs formatting, and 1 when an error occurred." ])
      ("",
       "--out",
       "<path>",
       [ "Write the result to this file or folder instead of formatting"
         "in place. Takes a single input path." ])
      ("",
       "--force",
       "",
       [ "Write the output even when it is not valid F# code."
         "For debugging purposes only." ])
      ("", "--profile", "", [ "Print the line count and the time taken for every file." ])
      ("", "--daemon", "", [ "Run an LSP-like server that editor tooling can talk to." ])
      ("-v",
       "--verbosity",
       "<level>",
       [ "How much to print: normal or detailed. Defaults to normal."
         "n and d are accepted as well." ])
      ("", "--version", "", [ "Print the version and exit" ])
      ("-h", "--help", "", [ "Display this menu and exit" ]) ]

let examples =
    [ ("fantomas .", "Format every F# file below the current folder")
      ("fantomas src/App.fs", "Format a single file in place")
      ("fantomas --check .", "Report what needs formatting, write nothing")
      ("fantomas --out build src", "Copy the formatted files to another folder") ]

let links =
    [ ("Learn more about Fantomas:", [ "https://fsprojects.github.io/fantomas/docs" ])
      ("Configure Fantomas:", [ "https://fsprojects.github.io/fantomas/docs/end-users/Configuration.html" ])
      ("Join the F# Discord:", [ "https://discord.com/channels/196693847965696000/1493226271767924747" ])
      ("Docs for your LLM:",
       [ "https://fsprojects.github.io/fantomas/llms.txt"
         "https://fsprojects.github.io/fantomas/llms-full.txt" ]) ]

let writeFlag (short: string, long: string, argument: string, description: string list) =
    let shortPart =
        if String.IsNullOrEmpty short then
            "    "
        else
            String.Concat(flagName short, ", ")

    let argumentPart =
        if String.IsNullOrEmpty argument then
            ""
        else
            String.Concat(" ", placeholder argument)

    let left = String.Concat("  ", shortPart, flagName long, argumentPart)

    match description with
    | [] -> write left
    | first :: rest ->
        writeRow descriptionColumn left first
        List.iter (writeContinuation descriptionColumn) rest

let writeExample (command: string, description: string) =
    let name, arguments =
        match command.IndexOf ' ' with
        | -1 -> command, ""
        | i -> command.Substring(0, i), command.Substring(i)

    writeRow exampleColumn (String.Concat("  ", muted name, flagName arguments)) description

let writeLink (label: string, urls: string list) =
    match urls with
    | [] -> ()
    | first :: rest ->
        writeRow linkColumn label (link first)
        List.iter (fun url -> writeContinuation linkColumn (link url)) rest

let print () =
    write (
        String.Concat(
            title "Fantomas",
            " is an opinionated source code formatter for F#. ",
            muted (String.Concat("(", shortVersion (), ")"))
        )
    )

    blank ()
    write (String.Concat(heading "Usage:", " ", heading "fantomas", " ", flagName "[...flags] [...paths]"))
    blank ()
    write (heading "Examples:")
    List.iter writeExample examples
    blank ()
    write (heading "Flags:")
    List.iter writeFlag flags
    blank ()
    write (heading "Paths:")
    write "  A path is a folder, which is searched recursively, or a file ending in .fs, .fsi,"
    write "  .fsx, .ml or .mli. Formatting settings are read from .editorconfig, and files"
    write "  matched by .fantomasignore in the current folder are skipped."
    blank ()
    List.iter writeLink links
    blank ()

// Argu builds a usage block of its own and hands it over as part of the message. Only the
// first line carries the actual complaint, so the rest is dropped in favour of a pointer to
// the page above.
let complaint (message: string) =
    message.Split('\n')
    |> Array.tryFind (fun line -> line.StartsWith("ERROR:", StringComparison.Ordinal))
    |> Option.defaultValue message
    |> fun line -> line.Trim()

let exiter =
    { new IExiter with
        member _.Name = "Fantomas help page"

        member _.Exit(message: string, errorCode: ErrorCode) =
            if errorCode = ErrorCode.HelpText then
                print ()
                exit 0
            else
                Console.Error.WriteLine(complaint message)
                Console.Error.WriteLine("Run fantomas --help for usage information.")
                exit (int errorCode) }
