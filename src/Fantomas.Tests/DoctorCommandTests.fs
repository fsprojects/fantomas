module Fantomas.Tests.DoctorCommandTests

open System.IO.Abstractions
open System.IO.Abstractions.TestingHelpers
open NUnit.Framework
open FsUnitTyped
open Fantomas
open Fantomas.Arguments
open Fantomas.DoctorCommand
open Fantomas.Tests.TestHelpers

let private write (fs: IFileSystem) (path: string) (content: string) : unit =
    fs.FileInfo.New(path).Directory.Create()
    fs.File.WriteAllText(path, content)

let private doctor (fs: IFileSystem) (ignoreFile: IgnoreFile option) (inputPath: InputPath) : DoctorCommandResult =
    let recorded: RecordedRun = recordingEnvironment fs ignoreFile
    runDoctorCommand recorded.Environment inputPath

let private completed (result: DoctorCommandResult) : DoctorReport =
    match result with
    | DoctorCommandResult.Completed report -> report
    | other -> failwith $"Expected the doctor to complete, got %A{other}"

/// A file in the mock root, and the report of putting it through every step.
let private diagnosing (content: string) : DoctorReport =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    write fs file content
    doctor fs None (InputPath.File file) |> completed

/// The doctor over a tree whose ignore files it resolves itself, rather than being handed the one
/// that governs. What is being tested is which ignore file was reached and which were passed over,
/// so handing one over would answer the question the test is asking.
let private doctorResolving (fs: IFileSystem) (inputPath: InputPath) : DoctorCommandResult =
    let recorded: RecordedRun = recordingEnvironment fs None

    let environment: Fantomas.Cli.CliEnvironment =
        { recorded.Environment with
            FindIgnoreFile = IgnoreFile.cachedFinder fs (IgnoreFile.loadIgnoreList fs)
            FindIgnoreFilesAbove = IgnoreFile.findAbove fs (IgnoreFile.loadIgnoreList fs)
        }

    runDoctorCommand environment inputPath

/// The ignore file at the mock root, resolved the way a run resolves it.
let private ignoreFileAt (fs: IFileSystem) (content: string) : IgnoreFile option =
    fs.File.WriteAllText(fs.Path.Combine(mockRoot fs, IgnoreFile.IgnoreFileName), content)
    IgnoreFile.findInDirectory fs (mockRoot fs) (IgnoreFile.loadIgnoreList fs)

// ---- counting lines and finding where two files part ----

[<Test>]
let ``a file has as many lines however it ends them`` () =
    lines "let a = 1\nlet b = 2\n" |> shouldEqual [| "let a = 1"; "let b = 2" |]
    lines "let a = 1\r\nlet b = 2\r\n" |> shouldEqual [| "let a = 1"; "let b = 2" |]

[<Test>]
let ``a last line with no newline after it is still a line`` () =
    lines "let a = 1" |> shouldEqual [| "let a = 1" |]
    lines "" |> shouldEqual Array.empty

[<Test>]
let ``two files that are the same part nowhere`` () =
    firstDifference [| "a"; "b" |] [| "a"; "b" |] |> shouldEqual None

[<Test>]
let ``two files part at the first line only one of them has`` () =
    // A second pass that only adds a line still changed the file, and the line it added is where
    // the two stop agreeing.
    firstDifference [| "a" |] [| "a"; "b" |] |> shouldEqual (Some 2)
    firstDifference [| "a"; "b" |] [| "a" |] |> shouldEqual (Some 2)
    firstDifference [| "a"; "x" |] [| "a"; "b" |] |> shouldEqual (Some 2)

// ---- the path itself ----

[<Test>]
let ``a path that is not there is the first step failing, and nothing after it is asked`` () =
    let fs: IFileSystem = MockFileSystem()
    let report: DoctorReport = doctor fs None (InputPath.NotFound "A.fs") |> completed

    // Resolved against the folder the run was started in, as every other path this command reports
    // is. "Not found" says nothing a reader can act on; "not found here" is what they need, and a
    // path that is not there resolves as readily as one that is.
    report.File |> shouldEqual (FileStep.NotFound(fs.Path.GetFullPath "A.fs"))
    report.Ignore |> shouldEqual None
    report.Settings |> shouldEqual None
    report.Format |> shouldEqual None

[<Test>]
let ``a path that is not there exits 1`` () =
    (doctor (MockFileSystem()) None (InputPath.NotFound "A.fs")).ExitCode
    |> shouldEqual 1

[<Test>]
let ``a file Fantomas does not format stops at the first step`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "README.md")
    write fs file "# hello"

    let result: DoctorCommandResult = doctor fs None (InputPath.NoFSharpFile file)

    match (completed result).File with
    | FileStep.NotFSharp _ -> ()
    | other -> failwith $"Expected the file not to be one Fantomas formats, got %A{other}"

    result.ExitCode |> shouldEqual 1

[<Test>]
let ``a folder is refused, because the answers differ per file`` () =
    let fs: IFileSystem = MockFileSystem()
    let folder: string = mockRoot fs

    match doctor fs None (InputPath.Folder folder) with
    | DoctorCommandResult.NotOneFile(InputPath.Folder given) -> given |> shouldEqual folder
    | other -> failwith $"Expected the folder to be refused, got %A{other}"

[<Test>]
let ``several paths are refused`` () =
    let fs: IFileSystem = MockFileSystem()

    match doctor fs None (InputPath.Multiple([ "A.fs"; "B.fs" ], [])) with
    | DoctorCommandResult.NotOneFile _ -> ()
    | other -> failwith $"Expected several paths to be refused, got %A{other}"

/// What the walk made of a file of this name, which is what decides how Fantomas parses it.
let private kindOf (fileName: string) (content: string) : FileKind =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, fileName)
    write fs file content

    match (doctor fs None (InputPath.File file) |> completed).File with
    | FileStep.Candidate file -> file.Kind
    | other -> failwith $"Expected a candidate, got %A{other}"

[<Test>]
let ``a signature file is named as one`` () =
    kindOf "A.fsi" "module A\n\nval a: int\n" |> shouldEqual FileKind.Signature

[<Test>]
let ``a script is named as one rather than as an implementation file`` () =
    kindOf "build.fsx" "let a = 1\n" |> shouldEqual FileKind.Script

[<Test>]
let ``an ml signature is named an implementation file, because that is what Fantomas parses it as`` () =
    // `.fsi` is the only extension the tool parses as a signature, here as everywhere else in it.
    // This reports what Fantomas does with a file rather than what the extension suggests.
    kindOf "A.mli" "let a = 1\n" |> shouldEqual FileKind.Implementation
    kindOf "A.fs" "let a = 1\n" |> shouldEqual FileKind.Implementation

[<Test>]
let ``a file a walk would never reach names the folder that keeps it out`` () =
    // The reason somebody with generated code under `obj` is looking at this command at all, and
    // it has nothing to do with their ignore file, which is where they were about to go and look.
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "obj", "Generated.fs")
    write fs file "let a = 1\n"

    match (doctor fs None (InputPath.File file) |> completed).File with
    | FileStep.Candidate file -> file.UnreachableUnder |> shouldEqual (Some(fs.Path.Combine(mockRoot fs, "obj")))
    | other -> failwith $"Expected a candidate, got %A{other}"

[<Test>]
let ``a file no compiler folder holds is reachable`` () =
    match (diagnosing "let a = 1\n").File with
    | FileStep.Candidate file -> file.UnreachableUnder |> shouldEqual None
    | other -> failwith $"Expected a candidate, got %A{other}"

// ---- the ignore file ----

[<Test>]
let ``no ignore file at or above the path is an answer of its own`` () =
    (diagnosing "let a = 1\n").Ignore |> shouldEqual (Some IgnoreStep.NoIgnoreFile)

[<Test>]
let ``an ignored file names the line of the ignore file that matched`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    write fs file "let  a =   1\n"

    let ignoreFile: IgnoreFile option = ignoreFileAt fs "# not these\n*.fsx\nA.fs\n"

    match (doctor fs ignoreFile (InputPath.File file) |> completed).Ignore with
    | Some(IgnoreStep.Governed(_, true, matches, _)) ->
        matches
        |> shouldEqual
            [
                {
                    LineNumber = 3
                    Pattern = "A.fs"
                    Negated = false
                }
            ]
    | other -> failwith $"Expected the file to be ignored, got %A{other}"

[<Test>]
let ``an ignored file stops the walk, because nothing below it happens to the file`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    write fs file "let  a =   1\n"

    let result: DoctorCommandResult =
        doctor fs (ignoreFileAt fs "A.fs\n") (InputPath.File file)

    let report: DoctorReport = completed result
    report.Settings |> shouldEqual None
    report.Format |> shouldEqual None
    report.Validity |> shouldEqual None
    report.Idempotency |> shouldEqual None

    // An answer, not a failure: the run this explains did exactly what it was told to.
    result.ExitCode |> shouldEqual 0

[<Test>]
let ``a pattern that takes a file back out is reported as the line that decided`` () =
    // The case nobody spots by eye, and the reason every matching line is carried rather than only
    // the verdict.
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    write fs file "let a = 1\n"

    match (doctor fs (ignoreFileAt fs "*.fs\n!A.fs\n") (InputPath.File file) |> completed).Ignore with
    | Some(IgnoreStep.Governed(_, false, matches, _)) ->
        matches
        |> List.map (fun m -> m.LineNumber, m.Pattern, m.Negated)
        |> shouldEqual [ (1, "*.fs", false); (2, "!A.fs", true) ]
    | other -> failwith $"Expected the file not to be ignored, got %A{other}"

[<Test>]
let ``a file no pattern matches is governed by the ignore file all the same`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    write fs file "let a = 1\n"

    match (doctor fs (ignoreFileAt fs "*.fsx\n") (InputPath.File file) |> completed).Ignore with
    | Some(IgnoreStep.Governed(_, false, [], _)) -> ()
    | other -> failwith $"Expected the file to be governed and unmatched, got %A{other}"

[<Test>]
let ``an ignore file the nearest one shadows is reported with what it would have decided`` () =
    // The whole reason the files above are looked for. Someone wrote `*.g.fs` at the root of their
    // repository, a subfolder has an ignore file of its own that says nothing about it, and the
    // root file is never opened. Naming the ignore file that did decide answers "which file" and
    // leaves "then why did mine not" for this.
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let file: string = fs.Path.Combine(root, "src", "Generated.g.fs")
    write fs file "let a = 1\n"
    fs.File.WriteAllText(fs.Path.Combine(root, IgnoreFile.IgnoreFileName), "*.g.fs\n")
    fs.File.WriteAllText(fs.Path.Combine(root, "src", IgnoreFile.IgnoreFileName), "Scratch.fs\n")

    match (doctorResolving fs (InputPath.File file) |> completed).Ignore with
    | Some(IgnoreStep.Governed(governing, false, [], [ above ])) ->
        governing
        |> shouldEqual (fs.Path.Combine(root, "src", IgnoreFile.IgnoreFileName))

        above.Path |> shouldEqual (fs.Path.Combine(root, IgnoreFile.IgnoreFileName))
        // What it would have made of the file, asked of it exactly as the governing file is asked.
        above.WouldIgnore |> shouldEqual true

        above.Matches
        |> List.map (fun (m: IgnoreMatch) -> m.LineNumber, m.Pattern)
        |> shouldEqual [ (1, "*.g.fs") ]
    | other -> failwith $"Expected one shadowed ignore file, got %A{other}"

[<Test>]
let ``the ordinary layout of one ignore file has nothing above it to report`` () =
    let fs: IFileSystem = MockFileSystem()
    let root: string = mockRoot fs
    let file: string = fs.Path.Combine(root, "src", "A.fs")
    write fs file "let a = 1\n"
    fs.File.WriteAllText(fs.Path.Combine(root, IgnoreFile.IgnoreFileName), "*.fsx\n")

    match (doctorResolving fs (InputPath.File file) |> completed).Ignore with
    | Some(IgnoreStep.Governed(_, false, [], shadowed)) -> shadowed |> shouldEqual []
    | other -> failwith $"Expected nothing above the governing ignore file, got %A{other}"

// ---- formatting, and what Fantomas makes of what it produced ----

[<Test>]
let ``nothing is written`` () =
    // The whole point of the command being its own thing: it is safe to run against a working tree
    // you have not committed.
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    let before: string = "let  a =   1\n"
    write fs file before

    doctor fs None (InputPath.File file) |> completed |> ignore

    fs.File.ReadAllText file |> shouldEqual before

[<Test>]
let ``a file that is already formatted changes no lines`` () =
    match (diagnosing "let a = 1\n").Format with
    | Some(FormatStep.Produced(_, change)) -> change |> shouldEqual FormatChange.Nothing
    | other -> failwith $"Expected formatting to produce something, got %A{other}"

[<Test>]
let ``a file that needs formatting says how many lines would change`` () =
    match (diagnosing "let  a =   1\nlet b = 2\n").Format with
    | Some(FormatStep.Produced(formatted, change)) ->
        change |> shouldEqual (FormatChange.Reformatted(1, 2))
        formatted |> shouldEqual "let a = 1\nlet b = 2\n"
    | other -> failwith $"Expected formatting to produce something, got %A{other}"

[<Test>]
let ``a file that needs formatting is not a failure`` () =
    // What would change is what `check` fails over. This command reports; it does not judge.
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    write fs file "let  a =   1\n"

    (doctor fs None (InputPath.File file)).ExitCode |> shouldEqual 0

[<Test>]
let ``a file whose only fault is its line endings is not reported as already formatted`` () =
    // The state that reads as already formatted to everything comparing line by line, and the one a
    // working tree checked out with the other platform's endings is in. A format run rewrites it,
    // so this command has to say so: the whole point of it is to agree with the run it explains.
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    write fs file "let a = 1\r\nlet b = 2\r\n"

    match (doctor fs None (InputPath.File file) |> completed).Format with
    | Some(FormatStep.Produced(formatted, change)) ->
        change |> shouldEqual FormatChange.LineEndingsOnly
        formatted |> shouldEqual "let a = 1\nlet b = 2\n"
    | other -> failwith $"Expected formatting to produce something, got %A{other}"

[<Test>]
let ``what the doctor says would change is what a format run changes`` () =
    // The two decide it the same way, which is the property that makes this command worth reading.
    // A file is put through both and they are made to agree about whether it would be rewritten.
    let wouldChange (content: string) : bool * bool =
        let fs: IFileSystem = MockFileSystem()
        let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
        write fs file content

        let saidByTheDoctor: bool =
            match (doctor fs None (InputPath.File file) |> completed).Format with
            | Some(FormatStep.Produced(_, FormatChange.Nothing)) -> false
            | Some(FormatStep.Produced _) -> true
            | other -> failwith $"Expected formatting to produce something, got %A{other}"

        let recorded: RecordedRun = recordingEnvironment fs None

        let doneByARun: bool =
            Fantomas.FormatCommand.runFormatCommand
                recorded.Environment
                defaultSettings
                (InputPath.File file)
                OutputPath.NotKnown
            |> function
                | Fantomas.CommandResult.FormatCommandResult.Completed [| Fantomas.CommandResult.FormatResult.Formatted _ |] ->
                    true
                | Fantomas.CommandResult.FormatCommandResult.Completed [| Fantomas.CommandResult.FormatResult.Unchanged _ |] ->
                    false
                | other -> failwith $"Expected one file to be formatted or left alone, got %A{other}"

        saidByTheDoctor, doneByARun

    for content in
        [
            "let a = 1\n" // nothing to do
            "let  a =   1\n" // whitespace to settle
            "let a = 1\r\nlet b = 2\r\n" // only the line endings
        ] do
        let saidByTheDoctor, doneByARun = wouldChange content
        saidByTheDoctor |> shouldEqual doneByARun

[<Test>]
let ``a file that will not parse fails at the format step and stops there`` () =
    let report: DoctorReport = diagnosing "let a = (1 + 2\n"

    match report.Format with
    | Some(FormatStep.Failed _) -> ()
    | other -> failwith $"Expected formatting to fail, got %A{other}"

    report.Validity |> shouldEqual None
    report.Idempotency |> shouldEqual None

[<Test>]
let ``a file that will not parse exits 1`` () =
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    write fs file "let a = (1 + 2\n"

    (doctor fs None (InputPath.File file)).ExitCode |> shouldEqual 1

[<Test>]
let ``output Fantomas accepts is reported as accepted`` () =
    (diagnosing "let  a =   1\n").Validity |> shouldEqual (Some ValidityStep.Valid)

[<Test>]
let ``formatting the result again is checked even when the first pass changed nothing`` () =
    // "The same text formats to itself" is what the step claims, and running it is the only thing
    // that establishes it.
    (diagnosing "let a = 1\n").Idempotency
    |> shouldEqual (Some IdempotencyStep.Idempotent)

[<Test>]
let ``every step of a file with nothing wrong with it is reached`` () =
    let report: DoctorReport = diagnosing "let  a =   1\n"

    report.Ignore.IsSome |> shouldEqual true
    report.Settings.IsSome |> shouldEqual true
    report.Format.IsSome |> shouldEqual true
    report.Validity.IsSome |> shouldEqual true
    report.Idempotency.IsSome |> shouldEqual true

// ---- what Fantomas would do to its own output ----
//
// Every case below is a bug in Fantomas, and no F# anybody can write makes a correct formatter
// produce one. They are reached by handing `walkFormatting` a formatter that does, which is what
// it takes one for. Whether the output parses is still asked of the real parser.

/// A walk from the format step onwards over `content`, formatted by `format`.
let private formattingWith (format: string -> string) (content: string) : DoctorReport =
    let start: DoctorReport =
        {
            File =
                FileStep.Candidate
                    {
                        Path = "/repo/A.fs"
                        Kind = FileKind.Implementation
                        LineCount = Array.length (lines content)
                        UnreachableUnder = None
                    }
            Ignore = Some IgnoreStep.NoIgnoreFile
            Settings = None
            Format = None
            Validity = None
            Idempotency = None
        }

    walkFormatting start false format content

[<Test>]
let ``output Fantomas will not accept is reported, and stops the walk before the second pass`` () =
    // Formatting the refused output again would report a parse failure in text nobody can open.
    let report: DoctorReport = formattingWith (fun _ -> "let a = (\n") "let a = 1\n"

    match report.Validity with
    | Some(ValidityStep.Invalid diagnostics) -> List.isEmpty diagnostics |> shouldEqual false
    | other -> failwith $"Expected the output to be refused, got %A{other}"

    report.Idempotency |> shouldEqual None

[<Test>]
let ``a second pass that changes the result names the line the two part at, and both of them`` () =
    let mutable passes: int = 0

    let format (_: string) : string =
        passes <- passes + 1

        if passes = 1 then
            "let a = 1\nlet b = 2\n"
        else
            "let a = 1\nlet b =  2\n"

    match (formattingWith format "let  a = 1\nlet b = 2\n").Idempotency with
    | Some(IdempotencyStep.NotIdempotent(line, afterFirst, afterSecond)) ->
        line |> shouldEqual 2
        afterFirst |> shouldEqual "let b = 2"
        afterSecond |> shouldEqual "let b =  2"
    | other -> failwith $"Expected the second pass to disagree, got %A{other}"

[<Test>]
let ``a second pass that adds a line parts at the line only one of them has`` () =
    let mutable passes: int = 0

    let format (_: string) : string =
        passes <- passes + 1

        if passes = 1 then
            "let a = 1\n"
        else
            "let a = 1\nlet b = 2\n"

    match (formattingWith format "let  a = 1\n").Idempotency with
    | Some(IdempotencyStep.NotIdempotent(line, afterFirst, afterSecond)) ->
        line |> shouldEqual 2
        // The first pass has no such line, so there is nothing of it to quote.
        afterFirst |> shouldEqual ""
        afterSecond |> shouldEqual "let b = 2"
    | other -> failwith $"Expected the second pass to disagree, got %A{other}"

[<Test>]
let ``a second pass that fails is reported as a failure of the second pass`` () =
    let mutable passes: int = 0

    let format (source: string) : string =
        passes <- passes + 1

        if passes = 1 then
            source
        else
            failwith "the second pass fell over"

    match (formattingWith format "let a = 1\n").Idempotency with
    | Some(IdempotencyStep.Failed error) -> error.Message |> shouldEqual "the second pass fell over"
    | other -> failwith $"Expected the second pass to fail, got %A{other}"

[<Test>]
let ``a file the formatter only adds to parts at the line only the result has`` () =
    match (formattingWith (fun _ -> "let a = 1\nlet b = 2\n") "let a = 1\n").Format with
    | Some(FormatStep.Produced(_, change)) -> change |> shouldEqual (FormatChange.Reformatted(2, 2))
    | other -> failwith $"Expected formatting to produce something, got %A{other}"

[<Test>]
let ``one line split into several is not reported as more lines than the file has`` () =
    // Counting lines that differ by position is not a count of edits: everything below a split
    // moves, and a file of five lines came back as `9 lines of 5 would change`. Where the two part
    // and how long the result is are both exact and neither can read as nonsense.
    let split: string = "let a = 1\nlet b = 2\nlet c = 3\n"

    match (formattingWith (fun _ -> "let a =\n    1\nlet b = 2\nlet c = 3\n") split).Format with
    | Some(FormatStep.Produced(_, change)) -> change |> shouldEqual (FormatChange.Reformatted(1, 4))
    | other -> failwith $"Expected formatting to produce something, got %A{other}"

// ---- what the run ends with ----

/// A report of a file that came through every step, for a test to break one step of.
let private healthy: DoctorReport =
    {
        File =
            FileStep.Candidate
                {
                    Path = "/repo/A.fs"
                    Kind = FileKind.Implementation
                    LineCount = 2
                    UnreachableUnder = None
                }
        Ignore = Some IgnoreStep.NoIgnoreFile
        Settings = None
        Format = Some(FormatStep.Produced("let a = 1\n", FormatChange.Nothing))
        Validity = Some ValidityStep.Valid
        Idempotency = Some IdempotencyStep.Idempotent
    }

let private exitCodeOf (report: DoctorReport) : int =
    (DoctorCommandResult.Completed report).ExitCode

[<Test>]
let ``a file with nothing wrong with it exits 0`` () = exitCodeOf healthy |> shouldEqual 0

[<Test>]
let ``every failure a step can find exits 1`` () =
    // Each of these is something somebody has to act on, which is what separates them from a file
    // that is merely ignored or merely in need of formatting.
    exitCodeOf
        { healthy with
            Format = Some(FormatStep.Failed(exn "could not be read"))
            Validity = None
            Idempotency = None
        }
    |> shouldEqual 1

    exitCodeOf
        { healthy with
            Validity = Some(ValidityStep.Invalid [])
            Idempotency = None
        }
    |> shouldEqual 1

    exitCodeOf
        { healthy with
            Idempotency = Some(IdempotencyStep.NotIdempotent(1, "a", "b"))
        }
    |> shouldEqual 1

    exitCodeOf
        { healthy with
            Idempotency = Some(IdempotencyStep.Failed(exn "the second pass fell over"))
        }
    |> shouldEqual 1

[<Test>]
let ``a failure no step can be blamed for is reported as one`` () =
    // The path was classified as a file and then could not be read, which is nothing the walk has a
    // step for and is not a reason to fall over without a word.
    let fs: IFileSystem = MockFileSystem()
    let missing: string = fs.Path.Combine(mockRoot fs, "Vanished.fs")

    match doctor fs None (InputPath.File missing) with
    | DoctorCommandResult.Failed _ -> ()
    | other -> failwith $"Expected the run to fail, got %A{other}"

    (doctor fs None (InputPath.File missing)).ExitCode |> shouldEqual 1

[<Test>]
let ``an ignore file with a pattern that will not compile is reported, not crashed on`` () =
    // The rules are compiled as the ignore file is read, so one unclosed bracket takes the file
    // with it before any question can be asked of it. That is a failure no step can be blamed for,
    // and the command answers with it rather than falling over: a run that ends in a stack trace
    // is the one thing a diagnostic command must not do.
    let fs: IFileSystem = MockFileSystem()
    let file: string = fs.Path.Combine(mockRoot fs, "A.fs")
    write fs file "let a = 1\n"
    fs.File.WriteAllText(fs.Path.Combine(mockRoot fs, IgnoreFile.IgnoreFileName), "[\n")

    let findIgnoreFile: string -> IgnoreFile option =
        IgnoreFile.cachedFinder fs (IgnoreFile.loadIgnoreList fs)

    let recorded: RecordedRun = recordingEnvironment fs None

    let result: DoctorCommandResult =
        runDoctorCommand
            { recorded.Environment with
                FindIgnoreFile = findIgnoreFile
            }
            (InputPath.File file)

    match result with
    | DoctorCommandResult.Failed error -> error.Message |> shouldContainText "Unterminated"
    | other -> failwith $"Expected the run to report the failure, got %A{other}"

    result.ExitCode |> shouldEqual 1
