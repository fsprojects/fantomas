module Fantomas.Tests.ProfileCommandTests

open System
open System.IO.Abstractions
open System.IO.Abstractions.TestingHelpers
open NUnit.Framework
open FsUnitTyped
open Fantomas
open Fantomas.Arguments
open Fantomas.CommandResult
open Fantomas.ProfileCommand
open Fantomas.Tests.TestHelpers

let private write (fs: IFileSystem) (path: string) (content: string) : unit =
    fs.FileInfo.New(path).Directory.Create()
    fs.File.WriteAllText(path, content)

let private profile (fs: IFileSystem) (ignoreFile: IgnoreFile option) (inputPath: InputPath) : ProfileCommandResult =
    let recorded: RecordedRun = recordingEnvironment fs ignoreFile
    runProfileCommand recorded.Environment inputPath

let private completed (result: ProfileCommandResult) : ProfileResult =
    match result with
    | ProfileCommandResult.Completed profile -> profile
    | other -> failwith $"Expected the profile to complete, got %A{other}"

[<Test>]
let ``a path that is not there is refused before anything is read`` () =
    profile (MockFileSystem()) None (InputPath.NotFound "A.fs")
    |> shouldEqual (ProfileCommandResult.InvalidInput(InputProblem.NotFound "A.fs"))

[<Test>]
let ``nothing is written`` () =
    // The whole point of the command being its own thing: it is safe to run against a working tree
    // you have not committed.
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    let before: string = "let  a =   1\n"
    write fs file before

    profile fs None (InputPath.File file) |> completed |> ignore

    fs.File.ReadAllText file |> shouldEqual before

[<Test>]
let ``every file that was formatted is timed`` () =
    let fs: IFileSystem = MockFileSystem()
    let a: string = fs.Path.Combine(mockRoot fs, "A.fs")
    let b: string = fs.Path.Combine(mockRoot fs, "B.fs")
    write fs a "let  a =   1\n"
    write fs b "let b = 2\n"

    let result: ProfileResult =
        profile fs None (InputPath.Folder(mockRoot fs)) |> completed

    result.Timings
    |> List.map (fun t -> t.File)
    |> List.sort
    |> shouldEqual [ a; b ]

    result.Errors |> shouldBeEmpty

[<Test>]
let ``the slowest file is first`` () =
    let fs: IFileSystem = MockFileSystem()

    for index in 1..6 do
        write fs (fs.Path.Combine(mockRoot fs, $"F%d{index}.fs")) (String.replicate index "let  a =   1\n")

    let result: ProfileResult =
        profile fs None (InputPath.Folder(mockRoot fs)) |> completed

    result.Timings
    |> List.pairwise
    |> List.forall (fun (earlier, later) -> earlier.TimeTaken >= later.TimeTaken)
    |> shouldEqual true

[<Test>]
let ``the line count does not depend on which line endings the file uses`` () =
    // It used to count occurrences of the platform's newline, so a file written with line feeds
    // counted zero lines on Windows and a file written with carriage returns counted zero on
    // anything else.
    let lineCountOf (content: string) : int =
        let fs: IFileSystem = MockFileSystem()
        let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
        write fs file content

        (profile fs None (InputPath.File file) |> completed).Timings
        |> List.exactlyOne
        |> fun timing -> timing.LineCount

    lineCountOf "let  a =   1\nlet  b =   2\n" |> shouldEqual 2
    lineCountOf "let  a =   1\r\nlet  b =   2\r\n" |> shouldEqual 2

[<Test>]
let ``a last line with no newline after it is still a line`` () =
    // Counting the line feeds alone left every file that does not end in a newline one line short,
    // and reported a file of one line as having none.
    let lineCountOf (content: string) : int =
        let fs: IFileSystem = MockFileSystem()
        let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
        write fs file content

        (profile fs None (InputPath.File file) |> completed).Timings
        |> List.exactlyOne
        |> fun timing -> timing.LineCount

    lineCountOf "let  a =   1" |> shouldEqual 1
    lineCountOf "let  a =   1\nlet  b =   2" |> shouldEqual 2

let private combinationsOf (content: string) : int =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    write fs file content

    (profile fs None (InputPath.File file) |> completed).Timings
    |> List.exactlyOne
    |> fun timing -> timing.DefineCombinations

[<Test>]
let ``a file with no conditional directives is formatted once`` () =
    combinationsOf "let a = 1\n" |> shouldEqual 1

[<Test>]
let ``a file with a conditional directive is formatted for every combination`` () =
    // This is what explains a short file costing more than a long one, so the report has to know
    // it rather than leave the reader to go and find the directive.
    combinationsOf "let a = 1\n#if DEBUG\nlet b = 2\n#endif\n" |> shouldEqual 2

[<Test>]
let ``a hash if inside a comment is still one combination`` () =
    // The count is only asked for when the text carries `#if` at all, because asking means parsing
    // the file a second time. A mention in a comment pays for that question and comes back with
    // the same answer, so the shortcut can cost time and cannot give a wrong count.
    combinationsOf "// mentions #if in a comment\nlet a = 1\n" |> shouldEqual 1

[<Test>]
let ``a file that cannot be parsed is an error rather than a timing`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "Bad.fs")
    write fs file "let a = (1 + 2\n"

    let result: ProfileResult = profile fs None (InputPath.File file) |> completed

    result.Timings |> shouldBeEmpty
    result.Errors |> List.map fst |> shouldEqual [ file ]

[<Test>]
let ``an errored file makes the run exit 1`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "Bad.fs")
    write fs file "let a = (1 + 2\n"

    (profile fs None (InputPath.File file)).ExitCode |> shouldEqual 1

[<Test>]
let ``an ignored file is named but not timed`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let file: string = fs.Path.Combine(root, "A.fs")
    write fs file "let  a =   1\n"
    fs.File.WriteAllText(fs.Path.Combine(root, Fantomas.IgnoreFile.IgnoreFileName), "A.fs")

    let ignoreFile: IgnoreFile option =
        Fantomas.IgnoreFile.findInDirectory fs root (Fantomas.IgnoreFile.loadIgnoreList fs)

    let result: ProfileResult = profile fs ignoreFile (InputPath.File file) |> completed

    result.Timings |> shouldBeEmpty
    result.Ignored |> shouldEqual [ file ]
