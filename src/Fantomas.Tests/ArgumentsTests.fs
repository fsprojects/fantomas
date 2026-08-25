module Fantomas.Tests.ArgumentsTests

open System.IO.Abstractions
open System.IO.Abstractions.TestingHelpers
open NUnit.Framework
open FsUnitTyped
open Fantomas.Arguments
open Fantomas.Logging
open Fantomas.Tests.TestHelpers

let private parsed (argv: string list) : Arguments list =
    match parse (Array.ofList argv) with
    | Ok given -> given
    | Error problem -> failwith $"Expected the command line to parse, got %A{problem}"

let private refused (argv: string list) : ArgumentProblem =
    match parse (Array.ofList argv) with
    | Error problem -> problem
    | Ok given -> failwith $"Expected the command line to be refused, got %A{given}"

[<Test>]
let ``flags and paths interleave in any order`` () =
    // The same command written three ways. What was asked for has to be the same however it was
    // typed, so these are compared without regard to the order the flags arrived in.
    let asked (argv: string list) : Arguments list =
        parsed argv |> List.sortBy describeArgument

    let expected: Arguments list =
        [ Arguments.Check; Arguments.Input [ "src"; "tests" ]; Arguments.Verbosity "d" ]
        |> List.sortBy describeArgument

    asked [ "--check"; "-v"; "d"; "src"; "tests" ] |> shouldEqual expected
    asked [ "src"; "--check"; "tests"; "-v"; "d" ] |> shouldEqual expected
    asked [ "-v"; "d"; "src"; "--check"; "tests" ] |> shouldEqual expected

[<Test>]
let ``a value is taken from the next token or attached with an equals`` () =
    parsed [ "--out"; "build" ] |> shouldEqual [ Arguments.Out "build" ]
    parsed [ "--out=build" ] |> shouldEqual [ Arguments.Out "build" ]
    parsed [ "-v=d" ] |> shouldEqual [ Arguments.Verbosity "d" ]

[<Test>]
let ``a token beginning with a dash is never taken as a value`` () =
    // `--out --check src` is a missing output path, not an output path named `--check`.
    refused [ "--out"; "--check"; "src" ]
    |> shouldEqual (ArgumentProblem.MissingValue("--out", Some "--check"))

[<Test>]
let ``a flag that takes a value and has none is refused`` () =
    refused [ "--out" ] |> shouldEqual (ArgumentProblem.MissingValue("--out", None))

[<Test>]
let ``a switch given a value is refused rather than quietly accepted`` () =
    refused [ "--check=true" ]
    |> shouldEqual (ArgumentProblem.UnexpectedValue("--check", "true"))

[<Test>]
let ``an unknown flag is reported as one rather than read as a path`` () =
    // It used to reach `classifyInputPath` and come back as "Input path '--nope' not found",
    // which sent the reader looking for a file.
    refused [ "--nope"; "src" ]
    |> shouldEqual (ArgumentProblem.UnknownFlag("--nope", None))

[<Test>]
let ``a misspelled flag names the one it is close to`` () =
    refused [ "--chek"; "src" ]
    |> shouldEqual (ArgumentProblem.UnknownFlag("--chek", Some "--check"))

[<Test>]
let ``an unknown short flag is not guessed at`` () =
    // A short flag is two characters, so one edit is half of it and every short flag is that close
    // to every other. `-x` came back as "did you mean -v", which is a guess dressed as help.
    refused [ "-x"; "src" ] |> shouldEqual (ArgumentProblem.UnknownFlag("-x", None))

[<Test>]
let ``a double dash ends the flags`` () =
    // The only way to name a file that begins with a dash, and it used to be read as a path
    // itself.
    parsed [ "--"; "--check"; "-v" ]
    |> shouldEqual [ Arguments.Input [ "--check"; "-v" ] ]

[<Test>]
let ``a lone dash is a path`` () =
    parsed [ "-" ] |> shouldEqual [ Arguments.Input [ "-" ] ]

[<Test>]
let ``repeating a flag is allowed and the last one wins`` () =
    // Argu refused this. A script that builds its arguments up should not fail on a duplicate,
    // and last-wins is what every other tool does.
    parsed [ "--check"; "--check" ] |> shouldEqual [ Arguments.Check ]

    parsed [ "-v"; "n"; "--verbosity"; "d" ]
    |> shouldEqual [ Arguments.Verbosity "d" ]

[<Test>]
let ``--out is the one flag that may not repeat`` () =
    // Everywhere else the last one wins, and nothing is lost by it. This one decides where files
    // are written, so choosing between two of them quietly is choosing which folder to write into.
    refused [ "--out"; "a"; "--out"; "b" ]
    |> shouldEqual (ArgumentProblem.RepeatedFlag "--out")

[<Test>]
let ``a repeated --out says what it decides, not only that it repeated`` () =
    describeArgumentProblem "dotnet fantomas" (ArgumentProblem.RepeatedFlag "--out")
    |> shouldEqual
        "'--out' was given more than once. It decides where files are written, so it takes one value rather than the last one given."

[<Test>]
let ``paths accumulate rather than replacing each other`` () =
    parsed [ "a.fs"; "b.fs"; "c.fs" ]
    |> shouldEqual [ Arguments.Input [ "a.fs"; "b.fs"; "c.fs" ] ]

[<Test>]
let ``an empty command line asks for nothing`` () = parsed [] |> shouldBeEmpty

[<Test>]
let ``the first token names the command when it names one`` () =
    splitCommand [| "profile"; "src" |]
    |> shouldEqual (Command.Profile, [| "src" |])

    splitCommand [| "profile" |] |> shouldEqual (Command.Profile, [||])

[<Test>]
let ``a token that names no command is left where it was`` () =
    // It is a path, the way it always was, so `fantomas src` still formats src.
    splitCommand [| "src" |] |> shouldEqual (Command.Format, [| "src" |])

    splitCommand [| "--check"; "src" |]
    |> shouldEqual (Command.Format, [| "--check"; "src" |])

    splitCommand [||] |> shouldEqual (Command.Format, [||])

[<Test>]
let ``a command is only the first token`` () =
    // Otherwise a file called `profile` could never be named.
    splitCommand [| "src"; "profile" |]
    |> shouldEqual (Command.Format, [| "src"; "profile" |])

[<Test>]
let ``what a profile run cannot be combined with`` () =
    argumentsRefusedBy
        Command.Profile
        [
            Arguments.Check
            Arguments.Out "build"
            Arguments.Force
            Arguments.Json
            Arguments.Daemon
            Arguments.Verbosity "d"
            Arguments.Input [ "src" ]
        ]
    // `--json` is kept: a machine asking for the timings should get them in the shape it reads.
    |> shouldEqual [ "--check"; "--daemon"; "--force"; "--out" ]

[<Test>]
let ``a format run refuses nothing`` () =
    // Every flag there is applies to formatting, which is why it is the command with no rule.
    argumentsRefusedBy Command.Format [ Arguments.Check; Arguments.Out "build"; Arguments.Input [ "src" ] ]
    |> shouldBeEmpty

[<Test>]
let ``the daemon can be asked for as a command`` () =
    // `--daemon` is the older spelling and goes on working: Fantomas.Client launches every one of
    // its three ways with it, so an editor talking to a newer Fantomas than itself still gets a
    // daemon. Unlike --profile, the flag means what the command means, so both can exist.
    splitCommand [| "daemon" |] |> shouldEqual (Command.Daemon, [||])

[<Test>]
let ``the check can be asked for as a command`` () =
    splitCommand [| "check"; "src" |] |> shouldEqual (Command.Check, [| "src" |])

[<Test>]
let ``both spellings of the check refuse the same arguments`` () =
    // `--check .` is in every pipeline there is, so the flag keeps working. It means what the
    // command means, so neither can refuse something the other allows.
    let refused: Arguments list = [ Arguments.Out "build"; Arguments.Force ]

    argumentsRefusedBy Command.Check (Arguments.Check :: refused)
    |> shouldEqual (argumentsRefusedBy Command.Check refused)

[<Test>]
let ``a check keeps the arguments it reports with`` () =
    // It reads the same files a format run does and reports the same two ways. What it has no use
    // for is where to put output and whether to write invalid code, because it writes nothing.
    argumentsRefusedBy Command.Check [ Arguments.Json; Arguments.Input [ "src" ]; Arguments.Verbosity "d" ]
    |> shouldBeEmpty

    argumentsRefusedBy Command.Check [ Arguments.Out "build"; Arguments.Force ]
    |> shouldEqual [ "--force"; "--out" ]

[<Test>]
let ``the doctor can be asked for as a command`` () =
    splitCommand [| "doctor"; "src/A.fs" |]
    |> shouldEqual (Command.Doctor, [| "src/A.fs" |])

[<Test>]
let ``a doctor keeps the arguments it reports with`` () =
    // It takes a path and reports the two ways every command reports. What it has no use for is
    // where to put output and whether to write invalid code, because it writes nothing.
    argumentsRefusedBy Command.Doctor [ Arguments.Json; Arguments.Input [ "src/A.fs" ]; Arguments.Verbosity "d" ]
    |> shouldBeEmpty

    argumentsRefusedBy Command.Doctor [ Arguments.Out "build"; Arguments.Force; Arguments.Check ]
    |> shouldEqual [ "--check"; "--force"; "--out" ]

[<Test>]
let ``both spellings of the daemon refuse the same arguments`` () =
    let refused: Arguments list =
        [ Arguments.Check; Arguments.Out "build"; Arguments.Input [ "src" ] ]

    argumentsRefusedBy Command.Daemon (Arguments.Daemon :: refused)
    |> shouldEqual (argumentsRefusedBy Command.Daemon refused)

[<Test>]
let ``every problem has its own wording, quoting what was typed`` () =
    [
        ArgumentProblem.UnknownFlag("--nope", None)
        ArgumentProblem.UnknownFlag("--chek", Some "--check")
        ArgumentProblem.MissingValue("--out", None)
        ArgumentProblem.MissingValue("--out", Some "--check")
        ArgumentProblem.UnexpectedValue("--check", "true")
        ArgumentProblem.UnreadableValue("--verbosity", "bogus", [ "normal"; "detailed"; "n"; "d" ])
    ]
    |> List.map (describeArgumentProblem "dotnet fantomas")
    |> shouldEqual
        [
            "'--nope' is not a Fantomas flag."
            "'--chek' is not a Fantomas flag. Did you mean '--check'?"
            "'--out' must be followed by a value."
            "'--out' must be followed by a value, but was followed by '--check'."
            "'--check' takes no value, but was given 'true'."
            "'--verbosity' does not accept 'bogus'. It accepts normal, detailed, n or d."
        ]

[<Test>]
let ``what a flag will take reads as a sentence rather than as a list`` () =
    describeArgumentProblem "dotnet fantomas" (ArgumentProblem.UnreadableValue("--x", "z", [ "a" ]))
    |> shouldContainText "It accepts a."

    describeArgumentProblem "dotnet fantomas" (ArgumentProblem.UnreadableValue("--x", "z", [ "a"; "b" ]))
    |> shouldContainText "It accepts a or b."

// The one message that names a command rather than a flag, and the one that could not be pinned
// while it asked the process what was running it.
[<Test>]
let ``a flag that is really a command names the command, spelled as this run was started`` () =
    describeArgumentProblem "dotnet fantomas" (ArgumentProblem.UnknownFlag("--profile", Some "profile"))
    |> shouldEqual "'--profile' is not a Fantomas flag. 'profile' is a command: try 'dotnet fantomas profile <paths>'."

// Every one of these used to be accepted alongside --daemon and then silently ignored.
[<Test>]
let ``the arguments that say what to format are refused alongside --daemon`` () =
    argumentsRefusedBy
        Command.Daemon
        [
            Arguments.Daemon
            Arguments.Check
            Arguments.Json
            Arguments.Force
            Arguments.Out "out"
            Arguments.Input [ "A.fs" ]
        ]
    |> shouldEqual [ "--check"; "--force"; "--json"; "--out"; "input paths" ]

// `--verbosity` sets the level the daemon logs at, so it is the one argument here that does
// something. `--version` is answered and exited on before this rule is ever asked.
[<Test>]
let ``--verbosity and --version are allowed alongside --daemon`` () =
    argumentsRefusedBy Command.Daemon [ Arguments.Daemon; Arguments.Verbosity "d"; Arguments.Version ]
    |> shouldBeEmpty

[<Test>]
let ``a daemon on its own is refused nothing`` () =
    argumentsRefusedBy Command.Daemon [ Arguments.Daemon ] |> shouldBeEmpty

// `--out a --out b` is two results and one complaint.
[<Test>]
let ``an argument given twice is named once`` () =
    argumentsRefusedBy Command.Daemon [ Arguments.Daemon; Arguments.Out "a"; Arguments.Out "b" ]
    |> shouldEqual [ "--out" ]

[<Test>]
let ``no input path at all is the current folder`` () =
    // A run that names no path names the folder it was started in, so there is no such thing as a
    // run with nothing to do. `ruff format` and `dotnet format` both read a bare invocation that
    // way, and refusing it taught nobody anything.
    classifyInputPath (MockFileSystem()) None |> shouldEqual (InputPath.Folder ".")

[<Test>]
let ``an empty list of input paths is the current folder too`` () =
    // The parser only builds `Input` when it has something to put in it, so this is defence rather
    // than a case anyone reaches; falling through to `Multiple` with nothing in it would not be.
    classifyInputPath (MockFileSystem()) (Some [])
    |> shouldEqual (InputPath.Folder ".")

[<Test>]
let ``a path that is not there is reported as not found`` () =
    let fs: IFileSystem = MockFileSystem()
    let missing: string = fs.Path.Combine(mockRoot fs, "nope.fs")

    classifyInputPath fs (Some [ missing ])
    |> shouldEqual (InputPath.NotFound missing)

[<Test>]
let ``an existing folder is a folder`` () =
    let fs: IFileSystem = MockFileSystem()
    let src: string = fs.Path.Combine(mockRoot fs, "src")
    [ fs.Path.Combine(src, "A.fs") ] |> makeFileHierarchy fs

    classifyInputPath fs (Some [ src ]) |> shouldEqual (InputPath.Folder src)

[<Test>]
let ``an existing F# file is a file`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "src", "A.fs")
    [ file ] |> makeFileHierarchy fs

    classifyInputPath fs (Some [ file ]) |> shouldEqual (InputPath.File file)

[<Test>]
let ``an existing file Fantomas does not format is reported as such`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "src", "README.md")
    [ file ] |> makeFileHierarchy fs

    classifyInputPath fs (Some [ file ])
    |> shouldEqual (InputPath.NoFSharpFile file)

[<Test>]
let ``several paths are told apart into files and folders`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let file: string = fs.Path.Combine(root, "A.fs")
    let folder: string = fs.Path.Combine(root, "src")
    [ file; fs.Path.Combine(folder, "B.fs") ] |> makeFileHierarchy fs

    classifyInputPath fs (Some [ file; folder ])
    |> shouldEqual (InputPath.Multiple([ file ], [ folder ]))

[<Test>]
let ``a folder with a dot in its name is still a folder`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let folder: string = fs.Path.Combine(root, "my.stuff")
    let file: string = fs.Path.Combine(root, "A.fs")
    [ file; fs.Path.Combine(folder, "B.fs") ] |> makeFileHierarchy fs

    // Guessing from the extension called this a file, and formatting it then failed.
    classifyInputPath fs (Some [ folder; file ])
    |> shouldEqual (InputPath.Multiple([ file ], [ folder ]))

[<Test>]
let ``a file with no extension is still a file`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let file: string = fs.Path.Combine(root, "Makefile")
    let folder: string = fs.Path.Combine(root, "src")
    [ file; fs.Path.Combine(folder, "B.fs") ] |> makeFileHierarchy fs

    classifyInputPath fs (Some [ file; folder ])
    |> shouldEqual (InputPath.Multiple([ file ], [ folder ]))

[<Test>]
let ``one missing path among several is reported rather than the rest being used`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let present: string = fs.Path.Combine(root, "A.fs")
    let missing: string = fs.Path.Combine(root, "B.fs")
    [ present ] |> makeFileHierarchy fs

    classifyInputPath fs (Some [ present; missing ])
    |> shouldEqual (InputPath.NotFound missing)

[<Test>]
let ``several paths keep the order they were given`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let a: string = fs.Path.Combine(root, "A.fs")
    let b: string = fs.Path.Combine(root, "B.fs")
    [ a; b ] |> makeFileHierarchy fs

    classifyInputPath fs (Some [ a; b ])
    |> shouldEqual (InputPath.Multiple([ a; b ], []))

[<Test>]
[<TestCase("n")>]
[<TestCase("normal")>]
[<TestCase("NORMAL")>]
let ``the spellings of normal verbosity`` (value: string) =
    parseVerbosity value |> shouldEqual (Some VerbosityLevel.Normal)

[<Test>]
[<TestCase("d")>]
[<TestCase("detailed")>]
[<TestCase("Detailed")>]
let ``detailed verbosity and its spellings`` (value: string) =
    parseVerbosity value |> shouldEqual (Some VerbosityLevel.Detailed)

[<Test>]
[<TestCase("")>]
[<TestCase("verbose")>]
[<TestCase("dd")>]
let ``a verbosity Fantomas does not know is refused`` (value: string) =
    parseVerbosity value |> shouldEqual None
