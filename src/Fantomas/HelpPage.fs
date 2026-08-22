module Fantomas.HelpPage

open System
open System.Text.RegularExpressions
open Argu
open Spectre.Console
open Fantomas.Core

[<RequireQualifiedAccess; Struct>]
type Palette =
    | NoColour
    | FourBit
    | EightBit

// Select graphic rendition sequences, so a decorated string can still be measured.
let escapeSequence: Regex = Regex(@"\u001b\[[0-9;]*m", RegexOptions.Compiled)

let detectPalette () : Palette =
    // What the terminal can do is Spectre.Console's answer to give: it knows the TERM values, it
    // honours NO_COLOR, and it reports which colour system is available.
    let capabilities: Capabilities = AnsiConsole.Profile.Capabilities

    // Redirection is decided here rather than left to Spectre. Spectre turns ANSI back on when it
    // detects a CI environment, because a CI log viewer renders escape codes and a progress bar
    // there is worth colouring. A help page is not: it gets piped into a file, a pager or a script
    // that reads it, on a build agent as much as anywhere else. Standard out being a terminal is
    // the question this page needs answered, so both have to agree.
    let colorsEnabled: bool =
        not Console.IsOutputRedirected
        && capabilities.Ansi
        && capabilities.ColorSystem <> ColorSystem.NoColors

    if not colorsEnabled then
        Palette.NoColour
    elif capabilities.ColorSystem >= ColorSystem.EightBit then
        Palette.EightBit
    else
        Palette.FourBit

// The palette is written out as escape codes rather than drawn by Spectre, because Spectre
// wraps what it writes to the console width and this page is laid out in fixed columns.
let decorate (palette: Palette) (eightBit: string) (fallback: string) (text: string) : string =
    match palette with
    | Palette.NoColour -> text
    | Palette.EightBit -> String.Concat("\u001b[", eightBit, "m", text, "\u001b[0m")
    | Palette.FourBit -> String.Concat("\u001b[", fallback, "m", text, "\u001b[0m")

// 38;5;38 is the closest 256 colour to the blue the website uses.
let title (palette: Palette) (text: string) : string =
    decorate palette "1;38;5;38" "1;36" text

let link (palette: Palette) (text: string) : string = decorate palette "38;5;38" "36" text
let heading (palette: Palette) (text: string) : string = decorate palette "1" "1" text

let flagName (palette: Palette) (text: string) : string =
    decorate palette "1;38;5;80" "1;36" text

let placeholder (palette: Palette) (text: string) : string = decorate palette "38;5;245" "2" text
let muted (palette: Palette) (text: string) : string = decorate palette "2" "2" text

// The column the right hand half of a two column row starts in.
let descriptionColumn: int = 29
let exampleColumn: int = 33
let linkColumn: int = 29

let visibleLength (text: string) : int = escapeSequence.Replace(text, "").Length

let writeRow (write: string -> unit) (column: int) (left: string) (right: string) : unit =
    let padding: string = String(' ', max 1 (column - visibleLength left))
    write (String.Concat(left, padding, right))

let writeContinuation (write: string -> unit) (column: int) (right: string) : unit =
    write (String.Concat(String(' ', column), right))

// Trim the commit hash the version carries down to the short form git itself shows.
let shortVersion () : string =
    let version: string = CodeFormatter.GetVersion()

    match version.Split('+') with
    | [| number; commit |] when commit.Length > 9 -> String.Concat(number, "+", commit.Substring(0, 9))
    | _ -> version

let flags: (string * string * string * string list) list =
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

let examples: (string * string) list =
    [ ("fantomas .", "Format every F# file below the current folder")
      ("fantomas src/App.fs", "Format a single file in place")
      ("fantomas --check .", "Report what needs formatting, write nothing")
      ("fantomas --out build src", "Copy the formatted files to another folder") ]

let links: (string * string list) list =
    [ ("Learn more about Fantomas:", [ "https://fsprojects.github.io/fantomas/docs" ])
      ("Configure Fantomas:", [ "https://fsprojects.github.io/fantomas/docs/end-users/Configuration.html" ])
      ("Join the F# Discord:", [ "https://discord.com/channels/196693847965696000/1493226271767924747" ])
      ("Docs for your LLM:",
       [ "https://fsprojects.github.io/fantomas/llms.txt"
         "https://fsprojects.github.io/fantomas/llms-full.txt" ]) ]

let writeFlag
    (write: string -> unit)
    (palette: Palette)
    (short: string, long: string, argument: string, description: string list)
    : unit =
    let shortPart: string =
        if String.IsNullOrEmpty short then
            "    "
        else
            String.Concat(flagName palette short, ", ")

    let argumentPart: string =
        if String.IsNullOrEmpty argument then
            ""
        else
            String.Concat(" ", placeholder palette argument)

    let left: string =
        String.Concat("  ", shortPart, flagName palette long, argumentPart)

    match description with
    | [] -> write left
    | first :: rest ->
        writeRow write descriptionColumn left first
        List.iter (writeContinuation write descriptionColumn) rest

let writeExample (write: string -> unit) (palette: Palette) (command: string, description: string) : unit =
    let name, arguments: string * string =
        match command.IndexOf ' ' with
        | -1 -> command, ""
        | i -> command.Substring(0, i), command.Substring(i)

    writeRow write exampleColumn (String.Concat("  ", muted palette name, flagName palette arguments)) description

let writeLink (write: string -> unit) (palette: Palette) (label: string, urls: string list) : unit =
    match urls with
    | [] -> ()
    | first :: rest ->
        writeRow write linkColumn label (link palette first)
        List.iter (fun (url: string) -> writeContinuation write linkColumn (link palette url)) rest

let render (palette: Palette) : string list =
    let lines: ResizeArray<string> = ResizeArray()
    let write (line: string) : unit = lines.Add line
    let blank () : unit = lines.Add ""

    write (
        String.Concat(
            title palette "Fantomas",
            " is an opinionated source code formatter for F#. ",
            muted palette (String.Concat("(", shortVersion (), ")"))
        )
    )

    blank ()

    write (
        String.Concat(
            heading palette "Usage:",
            " ",
            heading palette "fantomas",
            " ",
            flagName palette "[...flags] [...paths]"
        )
    )

    blank ()
    write (heading palette "Examples:")
    List.iter (writeExample write palette) examples
    blank ()
    write (heading palette "Flags:")
    List.iter (writeFlag write palette) flags
    blank ()
    write (heading palette "Paths:")
    write "  A path is a folder, which is searched recursively, or a file ending in .fs, .fsi,"
    write "  .fsx, .ml or .mli. Formatting settings are read from .editorconfig, and files"
    write "  matched by .fantomasignore in the current folder are skipped."
    blank ()
    List.iter (writeLink write palette) links
    blank ()
    List.ofSeq lines

let print () : unit =
    render (detectPalette ()) |> List.iter Console.Out.WriteLine

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
                exit (int errorCode) }
