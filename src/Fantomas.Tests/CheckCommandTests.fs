module Fantomas.Tests.CheckCommandTests

open System.IO.Abstractions
open System.IO.Abstractions.TestingHelpers
open NUnit.Framework
open FsUnitTyped
open Fantomas
open Fantomas.Arguments
open Fantomas.CheckCommand
open Fantomas.CommandResult
open Fantomas.Tests.TestHelpers

[<Literal>]
let private NeedsFormatting = "let  a =   1\n"

[<Literal>]
let private Formatted = "let a = 1\n"

let private write (fs: IFileSystem) (path: string) (content: string) : unit =
    fs.FileInfo.New(path).Directory.Create()
    fs.File.WriteAllText(path, content)

let private check (fs: IFileSystem) (ignoreFile: IgnoreFile option) (inputPath: InputPath) : CheckCommandResult =
    let recorded: RecordedRun = recordingEnvironment fs ignoreFile
    runCheckCommand recorded.Environment inputPath

let private completed (result: CheckCommandResult) : string list * CheckResult =
    match result with
    | CheckCommandResult.Completed(ignored, checkResult) -> ignored, checkResult
    | other -> failwith $"Expected the check to complete, got %A{other}"

[<Test>]
let ``a path that is not there is refused before anything is read`` () =
    check (MockFileSystem()) None (InputPath.NotFound "A.fs")
    |> shouldEqual (CheckCommandResult.InvalidInput(InputProblem.NotFound "A.fs"))

[<Test>]
let ``no input path is refused`` () =
    check (MockFileSystem()) None InputPath.Unspecified
    |> shouldEqual (CheckCommandResult.InvalidInput InputProblem.NoPathGiven)

[<Test>]
let ``a file Fantomas does not format is refused`` () =
    check (MockFileSystem()) None (InputPath.NoFSharpFile "A.md")
    |> shouldEqual (CheckCommandResult.InvalidInput(InputProblem.UnsupportedFileType "A.md"))

[<Test>]
let ``a file that is already formatted needs nothing`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    write fs file Formatted

    let ignored, result = check fs None (InputPath.File file) |> completed

    ignored |> shouldBeEmpty
    result.IsValid |> shouldEqual true

[<Test>]
let ``a file that cannot be parsed is an error and not also a file needing formatting`` () =
    // It used to be both, so one broken file was reported twice, once under each heading, and the
    // report told the reader to run a formatter that had already failed on it.
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "Bad.fs")
    write fs file "let a = (1 + 2\n"

    let _, result = check fs None (InputPath.File file) |> completed

    result.Errors |> List.map fst |> shouldEqual [ file ]
    result.Formatted |> shouldBeEmpty
    result.Unchanged |> shouldBeEmpty
    result.HasErrors |> shouldEqual true

[<Test>]
let ``a file that needs formatting is named`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    write fs file NeedsFormatting

    let _, result = check fs None (InputPath.File file) |> completed

    result.Formatted |> shouldEqual [ file ]
    result.HasErrors |> shouldEqual false
    result.NeedsFormatting |> shouldEqual true

[<Test>]
let ``a check writes nothing, whatever it finds`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    write fs file NeedsFormatting

    check fs None (InputPath.File file) |> completed |> ignore

    fs.File.ReadAllText file |> shouldEqual NeedsFormatting

[<Test>]
let ``a file that cannot be parsed is an error rather than a file needing formatting`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    write fs file "let a = ("

    let _, result = check fs None (InputPath.File file) |> completed

    result.HasErrors |> shouldEqual true
    result.Errors |> List.map fst |> shouldEqual [ file ]

[<Test>]
let ``every file below a folder is checked`` () =
    let fs: IFileSystem = MockFileSystem()
    let src: string = fs.Path.Combine(mockRoot fs, "src")
    let needs: string = fs.Path.Combine(src, "Needs.fs")
    write fs needs NeedsFormatting
    write fs (fs.Path.Combine(src, "nested", "Fine.fs")) Formatted

    let _, result = check fs None (InputPath.Folder src) |> completed

    result.Formatted |> shouldEqual [ needs ]

[<Test>]
let ``files and folders named together are all checked`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let loose: string = fs.Path.Combine(root, "Loose.fs")
    let src: string = fs.Path.Combine(root, "src")
    let inFolder: string = fs.Path.Combine(src, "A.fs")
    write fs loose NeedsFormatting
    write fs inFolder Formatted

    let _, result = check fs None (InputPath.Multiple([ loose ], [ src ])) |> completed

    result.Formatted |> shouldEqual [ loose ]

[<Test>]
let ``an ignored file is neither checked nor counted, and is reported as ignored`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let file: string = fs.Path.Combine(root, "A.fs")
    write fs file NeedsFormatting
    fs.File.WriteAllText(fs.Path.Combine(root, IgnoreFile.IgnoreFileName), "A.fs")

    let ignoreFile: IgnoreFile option =
        IgnoreFile.findInDirectory fs root (IgnoreFile.loadIgnoreList fs)

    let ignored, result = check fs ignoreFile (InputPath.File file) |> completed

    ignored |> shouldEqual [ file ]
    result.IsValid |> shouldEqual true

[<Test>]
let ``a difference in the number of newlines does make a file need formatting, 2461`` () =
    // Comparing without line endings strips carriage returns, not blank lines, so a trailing one
    // still counts.
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    write fs file "module A\n\n"

    let _, result = check fs None (InputPath.File file) |> completed

    result.NeedsFormatting |> shouldEqual true

[<Test>]
let ``line endings alone do not make a file need formatting`` () =
    // A check compares without line endings, so a file written with the other platform's
    // newlines is not reported as needing to be rewritten.
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    write fs file "let a = 1\r\n"

    let _, result = check fs None (InputPath.File file) |> completed

    result.IsValid |> shouldEqual true
