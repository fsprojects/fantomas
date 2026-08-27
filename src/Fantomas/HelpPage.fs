module Fantomas.HelpPage

open System
open Fantomas.Core
open Fantomas.Arguments
open Fantomas.Theme

// Trim the commit hash the version carries down to the short form git itself shows. Used by
// `--version` as well as by the page, so the two cannot come to say the version differently.
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
             "The older spelling of the check command above. Both do the"
             "same thing, and this one keeps working."
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
        ("",
         "--json",
         "",
         [
             "Report what the run did as one JSON document on standard out,"
             "naming what it looked at and positioning what went wrong. The"
             "usual messages are not printed; warnings go to standard error."
             "The shape is for reading, not for parsing against: it carries"
             "no version and may change in any release. The exit code is"
             "the part that is promised."
         ])
        ("",
         "--daemon",
         "",
         [
             "The older spelling of the daemon command above. Both do the"
             "same thing, and this one keeps working, which is what lets"
             "editor tooling built against an earlier Fantomas start this"
             "one."
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

// The commands a run can name, which is the first token when it names one. Each carries the
// `Command` it is about, so that `--help` after one of them can find the page to write.
let commands: (Command * string * string list) list =
    [
        (Command.Check,
         "check <paths>",
         [
             "Report which files need formatting and write nothing."
             "Exits 0 when every file is already formatted, 99 when some"
             "file needs formatting, and 1 when an error occurred."
         ])
        (Command.Profile,
         "profile <paths>",
         [
             "Report how long each file takes to format, slowest first."
             "Formats one file at a time so the timings can be compared,"
             "and writes nothing."
         ])
        (Command.Doctor,
         "doctor <file>",
         [
             "Walk one file through everything Fantomas does to it and"
             "report what happened at each step: whether it is a file"
             "Fantomas formats, which .fantomasignore governs it and which"
             "line of it decided, which settings apply and where each came"
             "from, what formatting produced, whether Fantomas accepts its"
             "own output, and whether formatting that output again leaves"
             "it alone. Takes one file rather than a folder, and writes"
             "nothing."
         ])
        (Command.Daemon,
         "daemon",
         [
             "Run an LSP-like server that editor tooling can talk to."
             "Takes no paths or other flags, apart from --verbosity."
         ])
    ]

// Whether a command has any use for a flag, asked of the rule that enforces it rather than of a
// second list kept beside it. A page that listed a flag the run would refuse, or left one off that
// it accepts, is exactly what a second list drifts into.
let appliesTo (command: Command) (long: string) : bool =
    match argumentFor long with
    | None -> false
    | Some argument -> List.isEmpty (argumentsRefusedBy command [ argument ])

// What a command's own page lists: the flags that change what it does, and nothing else.
//
// Two kinds are left out. A flag that answers and exits, `--version` and `--help`, is not a flag of
// any command and changes nothing about what this one would do; the overview is where somebody
// finds those. And a flag that is the older spelling of a command belongs on the overview too,
// where saying which is which is the point, rather than offering `--check` to somebody already
// running `check`.
let flagsFor (command: Command) : (string * string * string * string list) list =
    flags
    |> List.filter (fun (_, long: string, _, _) ->
        match argumentFor long with
        | None -> false
        | Some argument ->
            appliesTo command long
            && not (answersAndExits argument)
            && (commandSpelledBy argument |> Option.isNone)
    )

// Asked the same way, so a command that takes no paths does not carry a section about them.
let takesPaths (command: Command) : bool =
    List.isEmpty (argumentsRefusedBy command [ Arguments.Input [] ])

// What the paths a command takes are, said once. `doctor` gets its own wording because it takes
// one file where every other command takes any number of files and folders, and telling its reader
// that a folder is searched recursively would describe a run it will refuse.
let pathsSection (command: Command) : string list =
    match command with
    | Command.Doctor ->
        [
            "  One file, ending in .fs, .fsi, .fsx, .ml or .mli. A folder is refused: this reports"
            "  on the steps one file goes through, and the answers differ per file. Formatting"
            "  settings are read from .editorconfig, and the nearest .fantomasignore at or above"
            "  the file is the one that governs it."
        ]
    | Command.Format
    | Command.Check
    | Command.Profile
    | Command.Daemon ->
        [
            "  A path is a folder, which is searched recursively, or a file ending in .fs, .fsi,"
            "  .fsx, .ml or .mli. Naming none means the current folder. Formatting settings are"
            "  read from .editorconfig, and files matched by the nearest .fantomasignore at or"
            "  above them are skipped."
        ]

// What follows the command name, which is whatever this Fantomas was started as rather than a
// guess written into the page.
let examples: (string * string) list =
    [
        (".", "Format every F# file below the current folder")
        ("src/App.fs", "Format a single file in place")
        ("check .", "Report what needs formatting, write nothing")
        ("--out build src", "Copy the formatted files to another folder")
    ]

// The column the right hand half of a two column row starts in. The flags and the links are laid
// out against text this file owns, so those are fixed; the examples are laid out against a command
// name that is whatever this Fantomas was started as, so that one is measured.
let descriptionColumn: int = 29
let linkColumn: int = 29

let exampleColumn (invocation: string) : int =
    examples
    |> List.map (fun (arguments: string, _) -> invocation.Length + 1 + arguments.Length)
    |> List.fold max 0
    |> fun longest -> longest + 4

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

let writeExample
    (write: string -> unit)
    (theme: Theme)
    (invocation: string)
    ((arguments, description): string * string)
    : unit
    =
    writeRow
        write
        (exampleColumn invocation)
        (String.Concat("  ", muted theme invocation, flagName theme (String.Concat(" ", arguments))))
        description

let writeLink (write: string -> unit) (theme: Theme) (label: string, urls: string list) : unit =
    match urls with
    | [] -> ()
    | first :: rest ->

    writeRow write linkColumn label (link theme first)
    List.iter (fun (url: string) -> writeContinuation write linkColumn (link theme url)) rest

let renderOverview (theme: Theme) (invocation: string) : string list =
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
            heading theme invocation,
            " ",
            flagName theme "[command] [...flags] [...paths]"
        )
    )

    blank ()
    write (heading theme "Examples:")
    List.iter (writeExample write theme invocation) examples
    blank ()
    write (heading theme "Commands:")

    for _, name, description in commands do
        writeFlag write theme ("", name, "", description)

    blank ()
    write (heading theme "Flags:")

    // The flags that are older spellings of a command go last, and set apart. They still work and
    // still have to be findable, but nobody reaching this page for the first time should be
    // reading them before the ones to reach for.
    let current, older =
        flags
        |> List.partition (fun (_, long: string, _, _) ->
            argumentFor long |> Option.bind commandSpelledBy |> Option.isNone
        )

    List.iter (writeFlag write theme) current
    blank ()
    List.iter (writeFlag write theme) older
    blank ()
    write (heading theme "Paths:")
    List.iter write (pathsSection Command.Format)
    blank ()
    List.iter (writeLink write theme) links
    blank ()
    List.ofSeq lines

// A page about one command, listing only what that command has any use for. `fantomas daemon
// --help` carries neither `--out` nor a section about paths, because a daemon is refused both.
//
// No links either. Somebody reading a command's page has already found Fantomas and is asking a
// narrow question about one of its verbs; the documentation, the Discord and the llms files belong
// where somebody is still working out what the tool is, which is the overview.
let renderCommand
    (theme: Theme)
    (invocation: string)
    (command: Command)
    (name: string)
    (description: string list)
    : string list
    =
    let lines: ResizeArray<string> = ResizeArray()
    let write (line: string) : unit = lines.Add line
    let blank () : unit = lines.Add ""

    write (
        String.Concat(
            title theme (String.Concat(invocation, " ", name)),
            "  ",
            muted theme (String.Concat("(", shortVersion (), ")"))
        )
    )

    blank ()

    for line in description do
        write (String.Concat("  ", line))

    blank ()
    write (heading theme "Flags:")
    List.iter (writeFlag write theme) (flagsFor command)

    if takesPaths command then
        blank ()
        write (heading theme "Paths:")
        List.iter write (pathsSection command)

    blank ()
    List.ofSeq lines

let render (theme: Theme) (invocation: string) (command: Command) : string list =
    match commands |> List.tryFind (fun (named: Command, _, _) -> named = command) with
    | None -> renderOverview theme invocation
    | Some(_, name, description) -> renderCommand theme invocation command name description

let print (command: Command) : unit =
    render (forOutput ()) (Invocation.name ()) command
    |> List.iter Console.Out.WriteLine
