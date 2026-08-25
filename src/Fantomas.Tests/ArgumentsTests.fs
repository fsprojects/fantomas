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
    parsed [ "--out"; "a"; "--out"; "b" ] |> shouldEqual [ Arguments.Out "b" ]
    parsed [ "--check"; "--check" ] |> shouldEqual [ Arguments.Check ]

[<Test>]
let ``paths accumulate rather than replacing each other`` () =
    parsed [ "a.fs"; "b.fs"; "c.fs" ]
    |> shouldEqual [ Arguments.Input [ "a.fs"; "b.fs"; "c.fs" ] ]

[<Test>]
let ``an empty command line asks for nothing`` () = parsed [] |> shouldBeEmpty

[<Test>]
let ``nothing is refused when --daemon was not asked for`` () =
    // The guard belongs to the rule rather than to every caller of it. It used to sit in `main`,
    // which meant the function answered a question it had not been asked.
    argumentsRefusedWithDaemon [ Arguments.Check; Arguments.Out "build"; Arguments.Input [ "src" ] ]
    |> shouldBeEmpty

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
    |> List.map describeArgumentProblem
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
    describeArgumentProblem (ArgumentProblem.UnreadableValue("--x", "z", [ "a" ]))
    |> shouldContainText "It accepts a."

    describeArgumentProblem (ArgumentProblem.UnreadableValue("--x", "z", [ "a"; "b" ]))
    |> shouldContainText "It accepts a or b."

// Every one of these used to be accepted alongside --daemon and then silently ignored.
[<Test>]
let ``the arguments that say what to format are refused alongside --daemon`` () =
    argumentsRefusedWithDaemon
        [
            Arguments.Daemon
            Arguments.Check
            Arguments.Json
            Arguments.Force
            Arguments.Profile
            Arguments.Out "out"
            Arguments.Input [ "A.fs" ]
        ]
    |> shouldEqual [ "--check"; "--force"; "--json"; "--out"; "--profile"; "input paths" ]

// `--verbosity` sets the level the daemon logs at, so it is the one argument here that does
// something. `--version` is answered and exited on before this rule is ever asked.
[<Test>]
let ``--verbosity and --version are allowed alongside --daemon`` () =
    argumentsRefusedWithDaemon [ Arguments.Daemon; Arguments.Verbosity "d"; Arguments.Version ]
    |> shouldBeEmpty

[<Test>]
let ``a daemon on its own is refused nothing`` () =
    argumentsRefusedWithDaemon [ Arguments.Daemon ] |> shouldBeEmpty

// `--out a --out b` is two results and one complaint.
[<Test>]
let ``an argument given twice is named once`` () =
    argumentsRefusedWithDaemon [ Arguments.Daemon; Arguments.Out "a"; Arguments.Out "b" ]
    |> shouldEqual [ "--out" ]

[<Test>]
let ``no input path at all is unspecified`` () =
    classifyInputPath (MockFileSystem()) None |> shouldEqual InputPath.Unspecified

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
