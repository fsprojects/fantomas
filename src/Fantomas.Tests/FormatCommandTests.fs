module Fantomas.Tests.FormatCommandTests

open System.Text
open System.IO.Abstractions
open System.IO.Abstractions.TestingHelpers
open NUnit.Framework
open FsUnitTyped
open Fantomas
open Fantomas.Arguments
open Fantomas.Cli
open Fantomas.CommandResult
open Fantomas.FormatCommand
open Fantomas.Tests.TestHelpers

[<Literal>]
let private NeedsFormatting = "let  a =   1\n"

[<Literal>]
let private Formatted = "let a = 1\n"

/// Run the format command over the given file system, keeping what it wrote.
let private formatWith
    (settings: CliSettings)
    (ignoreFile: IgnoreFile option)
    (fs: IFileSystem)
    (inputPath: InputPath)
    (outputPath: OutputPath)
    : FormatCommandResult =
    let recorded: RecordedRun = recordingEnvironment fs ignoreFile
    runFormatCommand recorded.Environment settings inputPath outputPath

let private format (fs: IFileSystem) (inputPath: InputPath) (outputPath: OutputPath) : FormatCommandResult =
    formatWith defaultSettings None fs inputPath outputPath

/// Run the format command, keeping both the result and what it logged.
let private formatLogging
    (settings: CliSettings)
    (fs: IFileSystem)
    (inputPath: InputPath)
    (outputPath: OutputPath)
    : FormatCommandResult * CollectedLog =
    let recorded: RecordedRun = recordingEnvironment fs None

    let result: FormatCommandResult =
        runFormatCommand recorded.Environment settings inputPath outputPath

    result, recorded.Log()

let private write (fs: IFileSystem) (path: string) (content: string) : unit =
    fs.FileInfo.New(path).Directory.Create()
    fs.File.WriteAllText(path, content)

let private read (fs: IFileSystem) (path: string) : string =
    fs.File.ReadAllText path |> String.normalizeNewLine

let private results (result: FormatCommandResult) : FormatResult array =
    match result with
    | FormatCommandResult.Completed results -> results
    | other -> failwith $"Expected the run to complete, got %A{other}"

[<Test>]
let ``a badly formatted file is rewritten where it lies`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    write fs file NeedsFormatting

    let result: FormatResult array =
        format fs (InputPath.File file) OutputPath.NotKnown |> results

    result |> Array.length |> shouldEqual 1
    read fs file |> shouldEqual Formatted

[<Test>]
let ``a file that is already formatted is reported as unchanged`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    write fs file Formatted

    match format fs (InputPath.File file) OutputPath.NotKnown |> results with
    | [| FormatResult.Unchanged(f, _) |] -> f |> shouldEqual file
    | other -> failwith $"Expected one unchanged file, got %A{other}"

    read fs file |> shouldEqual Formatted

[<Test>]
let ``an output path is written to and the input is left alone`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let input: string = fs.Path.Combine(root, "A.fs")
    let output: string = fs.Path.Combine(root, "out", "A.fs")
    write fs input NeedsFormatting

    format fs (InputPath.File input) (OutputPath.IO output) |> results |> ignore

    read fs input |> shouldEqual NeedsFormatting
    read fs output |> shouldEqual Formatted

[<Test>]
let ``an output file whose folder does not exist yet is still written`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let input: string = fs.Path.Combine(root, "A.fs")
    let output: string = fs.Path.Combine(root, "not", "there", "yet", "A.fs")
    write fs input NeedsFormatting

    format fs (InputPath.File input) (OutputPath.IO output) |> results |> ignore

    read fs output |> shouldEqual Formatted

[<Test>]
let ``an output folder mirrors the input tree rather than flattening it`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let src: string = fs.Path.Combine(root, "src")
    let out: string = fs.Path.Combine(root, "out")
    // Same file name in two folders: flattening would let one overwrite the other.
    write fs (fs.Path.Combine(src, "A.fs")) "let  a =   1\n"
    write fs (fs.Path.Combine(src, "nested", "A.fs")) "let  b =   2\n"

    format fs (InputPath.Folder src) (OutputPath.IO out) |> results |> ignore

    read fs (fs.Path.Combine(out, "A.fs")) |> shouldEqual "let a = 1\n"
    read fs (fs.Path.Combine(out, "nested", "A.fs")) |> shouldEqual "let b = 2\n"

[<Test>]
let ``an output folder is created even when there is nothing to put in it`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let src: string = fs.Path.Combine(root, "src")
    let out: string = fs.Path.Combine(root, "out")
    fs.Directory.CreateDirectory src |> ignore

    format fs (InputPath.Folder src) (OutputPath.IO out) |> results |> ignore

    fs.Directory.Exists out |> shouldEqual true

[<Test>]
let ``an ignored file is not written and is reported as ignored`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let file: string = fs.Path.Combine(root, "A.fs")
    let output: string = fs.Path.Combine(root, "out", "A.fs")
    write fs file NeedsFormatting
    fs.File.WriteAllText(fs.Path.Combine(root, IgnoreFile.IgnoreFileName), "A.fs")

    let ignoreFile: IgnoreFile option =
        IgnoreFile.findInDirectory fs root (IgnoreFile.loadIgnoreList fs)

    let result: FormatResult array =
        formatWith defaultSettings ignoreFile fs (InputPath.File file) (OutputPath.IO output)
        |> results

    result |> shouldEqual [| FormatResult.IgnoredFile file |]
    read fs file |> shouldEqual NeedsFormatting
    fs.File.Exists output |> shouldEqual false

[<Test>]
let ``a file that cannot be parsed is reported as an error and left alone`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    let broken: string = "let a = ("
    write fs file broken

    match format fs (InputPath.File file) OutputPath.NotKnown |> results with
    | [| FormatResult.Error(f, _) |] -> f |> shouldEqual file
    | other -> failwith $"Expected one error, got %A{other}"

    read fs file |> shouldEqual broken

[<Test>]
let ``a byte order mark the input carried is put back on the output`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let input: string = fs.Path.Combine(root, "A.fs")
    let output: string = fs.Path.Combine(root, "out", "A.fs")
    fs.FileInfo.New(input).Directory.Create()
    fs.File.WriteAllText(input, NeedsFormatting, UTF8Encoding true)

    format fs (InputPath.File input) (OutputPath.IO output) |> results |> ignore

    let preamble: byte array = Encoding.UTF8.GetPreamble()

    fs.File.ReadAllBytes output
    |> Array.truncate preamble.Length
    |> shouldEqual preamble

[<Test>]
let ``a file without a byte order mark does not gain one`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let input: string = fs.Path.Combine(root, "A.fs")
    let output: string = fs.Path.Combine(root, "out", "A.fs")
    write fs input NeedsFormatting

    format fs (InputPath.File input) (OutputPath.IO output) |> results |> ignore

    let preamble: byte array = Encoding.UTF8.GetPreamble()

    fs.File.ReadAllBytes output
    |> Array.truncate preamble.Length
    |> shouldNotEqual preamble

[<Test>]
let ``several files and folders are all formatted where they lie`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let loose: string = fs.Path.Combine(root, "Loose.fs")
    let src: string = fs.Path.Combine(root, "src")
    let inFolder: string = fs.Path.Combine(src, "A.fs")
    write fs loose NeedsFormatting
    write fs inFolder NeedsFormatting

    format fs (InputPath.Multiple([ loose ], [ src ])) OutputPath.NotKnown
    |> results
    |> Array.length
    |> shouldEqual 2

    read fs loose |> shouldEqual Formatted
    read fs inFolder |> shouldEqual Formatted

[<Test>]
let ``content already at the output path is replaced rather than written over`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let input: string = fs.Path.Combine(root, "A.fs")
    let output: string = fs.Path.Combine(root, "out", "A.fs")
    write fs input NeedsFormatting
    // Opening the output without truncating it left the tail of whatever was there before.
    write fs output "// leftovers leftovers leftovers leftovers leftovers leftovers\n"

    format fs (InputPath.File input) (OutputPath.IO output) |> results |> ignore

    read fs output |> shouldEqual Formatted

[<Test>]
let ``with force, output that is not valid F# is written anyway`` () =
    // The day this fails because Fantomas can format the file is the day it can be deleted.
    let source: string =
        System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "tests", "data", "CheckDeclarations.fs")
        |> System.IO.Path.GetFullPath
        |> System.IO.File.ReadAllText

    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let input: string = fs.Path.Combine(root, "CheckDeclarations.fs")
    let output: string = fs.Path.Combine(root, "out", "CheckDeclarations.fs")
    write fs input source

    let settings: CliSettings = { defaultSettings with Force = true }

    match
        formatWith settings None fs (InputPath.File input) (OutputPath.IO output)
        |> results
    with
    | [| FormatResult.Formatted _ |] -> fs.File.Exists output |> shouldEqual true
    | other -> failwith $"Expected the invalid output to be written anyway, got %A{other}"

[<Test>]
let ``without force, output that is not valid F# is not written`` () =
    let source: string =
        System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "tests", "data", "CheckDeclarations.fs")
        |> System.IO.Path.GetFullPath
        |> System.IO.File.ReadAllText

    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let input: string = fs.Path.Combine(root, "CheckDeclarations.fs")
    let output: string = fs.Path.Combine(root, "out", "CheckDeclarations.fs")
    write fs input source

    // Without force the invalid output is turned into an error rather than reported as invalid,
    // so that the caller has one kind of failure to report rather than two.
    match format fs (InputPath.File input) (OutputPath.IO output) |> results with
    | [| FormatResult.Error(f, error) |] ->
        f |> shouldEqual input
        error.Message |> shouldContainText "leads to invalid F# code"
        fs.File.Exists output |> shouldEqual false
    | other -> failwith $"Expected the invalid output to be withheld, got %A{other}"

[<Test>]
let ``a run says what it is doing at detailed verbosity`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let input: string = fs.Path.Combine(root, "A.fs")
    let output: string = fs.Path.Combine(root, "out", "A.fs")
    write fs input NeedsFormatting

    let _, log =
        formatLogging defaultSettings fs (InputPath.File input) (OutputPath.IO output)

    log.Debug |> shouldContain $"Processing %s{input}"
    log.Debug |> shouldContain $"%s{output} has been written."

[<Test>]
let ``an unchanged file says so at detailed verbosity`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    write fs file Formatted

    let _, log =
        formatLogging defaultSettings fs (InputPath.File file) OutputPath.NotKnown

    log.Debug |> shouldContain $"'%s{file}' was unchanged"

[<Test>]
let ``with force, the output being invalid is said out loud`` () =
    let source: string =
        System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "tests", "data", "CheckDeclarations.fs")
        |> System.IO.Path.GetFullPath
        |> System.IO.File.ReadAllText

    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let input: string = fs.Path.Combine(root, "CheckDeclarations.fs")
    let output: string = fs.Path.Combine(root, "out", "CheckDeclarations.fs")
    write fs input source

    let settings: CliSettings = { defaultSettings with Force = true }
    let _, log = formatLogging settings fs (InputPath.File input) (OutputPath.IO output)

    log.Information |> shouldContain $"%s{input} was not valid after formatting."

[<Test>]
let ``profiling collects a line count and a time for the file it formatted`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    write fs file "let  a =   1\nlet  b =   2\n"

    let settings: CliSettings = { defaultSettings with Profile = true }

    match formatWith settings None fs (InputPath.File file) OutputPath.NotKnown |> results with
    | [| FormatResult.Formatted(_, _, Some profile) |] -> profile.LineCount |> shouldBeGreaterThan 0
    | other -> failwith $"Expected a profiled result, got %A{other}"

[<Test>]
let ``the line count does not depend on which line endings the file uses`` () =
    // It used to count occurrences of the platform's newline, so a file written with line feeds
    // counted zero lines on Windows and a file written with carriage returns counted zero on
    // anything else.
    let lineCountOf (content: string) : int =
        let fs: IFileSystem = MockFileSystem()
        let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
        write fs file content

        let settings: CliSettings = { defaultSettings with Profile = true }

        match formatWith settings None fs (InputPath.File file) OutputPath.NotKnown |> results with
        | [| FormatResult.Formatted(_, _, Some profile) |] -> profile.LineCount
        | other -> failwith $"Expected a profiled result, got %A{other}"

    lineCountOf "let  a =   1\nlet  b =   2\n" |> shouldEqual 2
    lineCountOf "let  a =   1\r\nlet  b =   2\r\n" |> shouldEqual 2

[<Test>]
let ``nothing is profiled unless profiling was asked for`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    write fs file NeedsFormatting

    match format fs (InputPath.File file) OutputPath.NotKnown |> results with
    | [| FormatResult.Formatted(_, _, None) |] -> ()
    | other -> failwith $"Expected no profile, got %A{other}"

[<Test>]
let ``an unusable input path never reaches the file system`` () =
    let fs: IFileSystem = MockFileSystem()

    format fs (InputPath.NotFound "A.fs") OutputPath.NotKnown
    |> shouldEqual (FormatCommandResult.InvalidInput(InputProblem.NotFound "A.fs"))
