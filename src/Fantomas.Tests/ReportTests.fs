module Fantomas.Tests.ReportTests

open System
open System.IO.Abstractions.TestingHelpers
open NUnit.Framework
open FsUnitTyped
open Fantomas.Core
open Fantomas.FCS.Text
open Fantomas.Arguments
open Fantomas.CommandResult
open Fantomas.Logging
open Fantomas.Report
open Fantomas.Tests.TestHelpers

let private run () : RecordedRun =
    recordingEnvironment (MockFileSystem()) None

/// The folder a test's results are pretended to have come from, named back in the messages that
/// have to name it. Which path it is does not matter to any assertion here.
let private inputFolder: InputPath = InputPath.Folder "src"

/// Output Fantomas would not accept, and what it would not accept about it. Built here rather than
/// produced by formatting something, since what these tests are about is the reporting.
let private rejectedOutput: string = "module A\n\nlet a = (1\n"

let private rejection: Fantomas.FCS.Parse.FSharpParserDiagnostic list =
    [
        {
            Severity = Fantomas.FCS.Diagnostics.FSharpDiagnosticSeverity.Error
            SubCategory = "parse"
            Range = Some(Range.mkRange "tmp.fsx" (Position.mkPos 3 9) (Position.mkPos 3 10))
            ErrorNumber = Some 583
            Message = "Unmatched '('"
        }
    ]

let private reportFormatTo
    (outputPath: OutputPath)
    (settings: Fantomas.Cli.CliSettings)
    (result: FormatCommandResult)
    : int * CollectedLog
    =
    let recorded: RecordedRun = run ()

    let code: int =
        reportFormatCommand recorded.Environment settings inputFolder outputPath result

    code, recorded.Log()

let private reportFormat (settings: Fantomas.Cli.CliSettings) (result: FormatCommandResult) : int * CollectedLog =
    reportFormatTo OutputPath.NotKnown settings result

let private reportCheck (result: CheckCommandResult) : int * CollectedLog =
    let recorded: RecordedRun = run ()
    let code: int = reportCheckCommand recorded.Environment inputFolder result
    code, recorded.Log()

/// Report against paths the caller is pretended to have typed, rather than against a folder.
let private reportFormatOf (inputPath: InputPath) (result: FormatCommandResult) : CollectedLog =
    let recorded: RecordedRun = run ()

    reportFormatCommand recorded.Environment defaultSettings inputPath OutputPath.NotKnown result
    |> ignore

    recorded.Log()

let private reportCheckOf (inputPath: InputPath) (result: CheckCommandResult) : CollectedLog =
    let recorded: RecordedRun = run ()
    reportCheckCommand recorded.Environment inputPath result |> ignore
    recorded.Log()

[<Test>]
let ``every way the input paths can fail has its own wording`` () =
    [
        InputProblem.UnsupportedFileType "A.md"
        InputProblem.NotFound "A.fs"
        InputProblem.MultiplePathsWithOut
    ]
    |> List.map describeInputProblem
    |> shouldEqual
        [
            "Input path 'A.md' is an unsupported file type."
            "Input path 'A.fs' not found."
            "Multiple input files are not supported with the --out flag."
        ]

[<Test>]
let ``an unusable input path is reported on error and exits 1`` () =
    let code, log =
        reportFormat defaultSettings (FormatCommandResult.InvalidInput(InputProblem.NotFound "A.fs"))

    code |> shouldEqual 1
    log.Error |> shouldEqual [ "Input path 'A.fs' not found." ]
    log.Information |> shouldBeEmpty

[<Test>]
let ``a failure no single file can be blamed for is reported and exits 1`` () =
    let code, log =
        reportFormat defaultSettings (FormatCommandResult.Failed(Exception "the disk went away"))

    code |> shouldEqual 1
    log.Error |> shouldEqual [ "the disk went away" ]

[<Test>]
let ``a single formatted file is reported as a sentence naming it`` () =
    let code, log =
        reportFormat defaultSettings (FormatCommandResult.Completed [| FormatResult.Formatted("A.fs", "let a = 1") |])

    code |> shouldEqual 0
    log.Information |> shouldEqual [ "+ A.fs was formatted." ]

[<Test>]
let ``a file is reported by the path it was given, not by its name alone`` () =
    // Reducing the path to its file name is the regression this guards: `sub/A.fs` has to come
    // back as `sub/A.fs`, whatever spelling the caller used.
    let path: string = "sub/A.fs"

    let formatted, log =
        reportFormat defaultSettings (FormatCommandResult.Completed [| FormatResult.Formatted(path, "") |])

    let _, unchangedLog =
        reportFormat defaultSettings (FormatCommandResult.Completed [| FormatResult.Unchanged path |])

    let _, ignoredLog =
        reportFormat defaultSettings (FormatCommandResult.Completed [| FormatResult.IgnoredFile path |])

    formatted |> shouldEqual 0
    log.Information |> shouldEqual [ "+ sub/A.fs was formatted." ]
    unchangedLog.Information |> shouldEqual [ "= sub/A.fs was unchanged." ]

    ignoredLog.Information
    |> shouldEqual [ "- sub/A.fs was ignored by .fantomasignore." ]

[<Test>]
let ``a file that failed is reported by the path it was given`` () =
    let _, log =
        reportFormat
            defaultSettings
            (FormatCommandResult.Completed [| FormatResult.Error("sub/A.fs", Exception "boom") |])

    log.Error |> shouldEqual [ "x sub/A.fs could not be formatted: boom" ]

[<Test>]
let ``a check names the files it found by the path it was given`` () =
    let _, log =
        reportCheck (
            CheckCommandResult.Completed(
                [],
                {
                    Errors = []
                    Formatted = [ "sub/A.fs" ]
                    Unchanged = []
                }
            )
        )

    // One file, so no counts: the line above already named it and said what was found, and the
    // format command answers a single file the same way.
    log.Information
    |> shouldEqual [ "! sub/A.fs needs formatting."; ""; "Run dotnet fantomas src to format it." ]

[<Test>]
let ``the files a run left alone are named at detailed verbosity`` () =
    // A folder run only counts what it did not change, so this is the one place it names them. It
    // used to be said where the file was formatted, which did not know whether the sentence below
    // had already said it.
    let _, log =
        reportFormat
            defaultSettings
            (FormatCommandResult.Completed [| FormatResult.Formatted("A.fs", ""); FormatResult.Unchanged "B.fs" |])

    log.Debug |> shouldEqual [ "'B.fs' was unchanged" ]

[<Test>]
let ``one file does not have its state said twice`` () =
    // `'A.fs' was unchanged` at Debug and `= A.fs was unchanged.` at Information both printed for a
    // single file at detailed verbosity: one event in two spellings.
    let _, log =
        reportFormat defaultSettings (FormatCommandResult.Completed [| FormatResult.Unchanged "A.fs" |])

    log.Information |> shouldEqual [ "= A.fs was unchanged." ]
    log.Debug |> shouldBeEmpty

[<Test>]
let ``the files a run ignored are named at detailed verbosity`` () =
    // A single result is named outright; among several, only what changed is listed, so this
    // debug line is the only place a folder run says which files it skipped.
    let _, log =
        reportFormat
            defaultSettings
            (FormatCommandResult.Completed [| FormatResult.Formatted("A.fs", ""); FormatResult.IgnoredFile "sub/B.fs" |])

    log.Debug |> shouldEqual [ "'sub/B.fs' was ignored" ]

[<Test>]
let ``a single unchanged file is reported as unchanged`` () =
    let code, log =
        reportFormat defaultSettings (FormatCommandResult.Completed [| FormatResult.Unchanged "A.fs" |])

    code |> shouldEqual 0
    log.Information |> shouldEqual [ "= A.fs was unchanged." ]

[<Test>]
let ``a single ignored file is reported as ignored`` () =
    let code, log =
        reportFormat defaultSettings (FormatCommandResult.Completed [| FormatResult.IgnoredFile "A.fs" |])

    code |> shouldEqual 0
    log.Information |> shouldEqual [ "- A.fs was ignored by .fantomasignore." ]

[<Test>]
let ``a single file that failed is reported on error and exits 1`` () =
    let code, log =
        reportFormat defaultSettings (FormatCommandResult.Completed [| FormatResult.Error("A.fs", Exception "boom") |])

    code |> shouldEqual 1
    log.Error |> shouldEqual [ "x A.fs could not be formatted: boom" ]

[<Test>]
let ``code that came out invalid is reported as a failure and exits 1`` () =
    let code, log =
        reportFormat
            defaultSettings
            (FormatCommandResult.Completed [| FormatResult.InvalidCode("A.fs", rejectedOutput, rejection) |])

    code |> shouldEqual 1

    // One report, and the file named once inside it. It used to be named twice, once by the
    // reporter and again inside the message the exception carried.
    let report: string = log.Error |> List.exactlyOne
    report.Split("A.fs").Length - 1 |> shouldEqual 1

    report.Split('\n')
    |> Array.head
    |> shouldEqual "x A.fs could not be formatted by Fantomas:"

    // The four things the report exists to say: whose fault this is, that nothing was lost, where
    // to take it, and what was wrong with the output. It used to say only that the code was not
    // valid, which reads as though the file were at fault and leaves the reader with nothing to do
    // but run again with `--force` and find it themselves.
    report |> shouldContainText "a bug in Fantomas"
    report |> shouldContainText "your file is unchanged"
    report |> shouldContainText "https://fsprojects.github.io/fantomas-tools/"
    report |> shouldContainText "error FS0583: Unmatched '('"
    report |> shouldContainText "3 | let a = (1"

[<Test>]
let ``a failure says what went wrong at any verbosity`` () =
    // It used to say only that the file could not be formatted, and put the one sentence explaining
    // it behind `--verbosity d`: an unreadable file reported nothing about permissions.
    let code, log =
        reportFormat defaultSettings (FormatCommandResult.Completed [| FormatResult.Error("A.fs", Exception "boom") |])

    code |> shouldEqual 1
    log.Error |> List.exactlyOne |> shouldContainText "boom"

[<Test>]
let ``a detailed run adds the whole exception below the line to act on`` () =
    let settings: Fantomas.Cli.CliSettings =
        { defaultSettings with
            Verbosity = VerbosityLevel.Detailed
        }

    let code, log =
        reportFormat settings (FormatCommandResult.Completed [| FormatResult.Error("A.fs", Exception "boom") |])

    code |> shouldEqual 1
    log.Error |> shouldEqual [ "x A.fs could not be formatted: boom" ]
    log.Debug |> List.exactlyOne |> shouldContainText "System.Exception: boom"

[<Test>]
let ``several files are reported as what changed and a line of counts`` () =
    let code, log =
        reportFormat
            defaultSettings
            (FormatCommandResult.Completed
                [|
                    FormatResult.Formatted("A.fs", "")
                    FormatResult.Unchanged "B.fs"
                    FormatResult.IgnoredFile "C.fs"
                |])

    code |> shouldEqual 0

    // Only what changed is listed. The rest is a count, which is what keeps a run over an already
    // formatted tree to a single line. A state at zero is left out, so `errored` is absent here.
    log.Information
    |> shouldEqual [ "+ A.fs was formatted."; ""; "1 file formatted, 1 unchanged." ]

[<Test>]
let ``a run that found no F# files says so rather than exiting quietly`` () =
    // Silence and exit 0 is what a bad glob or an over broad ignore file used to look like, which
    // reads as a green build forever.
    let code, log = reportFormat defaultSettings (FormatCommandResult.Completed [||])

    code |> shouldEqual 0
    log.Information |> shouldBeEmpty
    log.Warning |> shouldEqual [ "No F# files found in src." ]

[<Test>]
let ``a run that ignored every file it found says so`` () =
    let code, log =
        reportFormat
            defaultSettings
            (FormatCommandResult.Completed [| FormatResult.IgnoredFile "A.fs"; FormatResult.IgnoredFile "B.fs" |])

    code |> shouldEqual 0
    log.Information |> shouldBeEmpty

    log.Warning
    |> shouldEqual [ "All 2 F# files in src were ignored by .fantomasignore." ]

[<Test>]
let ``writing somewhere else counts what was written rather than what changed`` () =
    // Under --out nothing is really unchanged: every input produces an output file, so the count
    // that means something is how many were written.
    let _, log =
        reportFormatTo
            (OutputPath.IO "build")
            defaultSettings
            (FormatCommandResult.Completed
                [|
                    FormatResult.Formatted("A.fs", "")
                    FormatResult.Unchanged "B.fs"
                    FormatResult.Unchanged "C.fs"
                |])

    log.Information
    |> shouldEqual [ "+ A.fs was formatted."; ""; "3 files written to build, 1 reformatted." ]

[<Test>]
let ``writing somewhere else still counts what failed`` () =
    // A run where every file failed came out as a lone full stop, `written` and `reformatted` both
    // being zero and nothing else being counted.
    let _, log =
        reportFormatTo
            (OutputPath.IO "build")
            defaultSettings
            (FormatCommandResult.Completed
                [|
                    FormatResult.Formatted("A.fs", "")
                    FormatResult.IgnoredFile "B.fs"
                    FormatResult.Error("C.fs", Exception "boom")
                |])

    log.Information
    |> shouldEqual
        [
            "+ A.fs was formatted."
            ""
            "1 file written to build, 1 reformatted, 1 errored."
        ]

[<Test>]
let ``writing somewhere else where nothing could be written still says so`` () =
    // With only `written` and `reformatted` counted, both were zero here and the whole summary came
    // out as a lone full stop on standard out.
    let _, log =
        reportFormatTo
            (OutputPath.IO "build")
            defaultSettings
            (FormatCommandResult.Completed
                [|
                    FormatResult.Error("A.fs", Exception "boom")
                    FormatResult.Error("B.fs", Exception "boom")
                |])

    log.Information |> shouldEqual [ "2 files errored." ]

[<Test>]
let ``a single file written somewhere else names where it went`` () =
    // Under `--out` a file appears at the destination whether or not its content changed, so a line
    // saying only `was unchanged` left the run's whole effect unmentioned.
    let _, formattedLog =
        reportFormatTo
            (OutputPath.IO "build/A.fs")
            defaultSettings
            (FormatCommandResult.Completed [| FormatResult.Formatted("A.fs", "") |])

    let _, unchangedLog =
        reportFormatTo
            (OutputPath.IO "build/A.fs")
            defaultSettings
            (FormatCommandResult.Completed [| FormatResult.Unchanged "A.fs" |])

    formattedLog.Information
    |> shouldEqual [ "+ A.fs was formatted and written to build/A.fs." ]

    unchangedLog.Information
    |> shouldEqual [ "= A.fs was written to build/A.fs unchanged." ]

[<Test>]
let ``a single file that was skipped or failed names no destination`` () =
    // Nothing was written, so there is nowhere to name.
    let _, log =
        reportFormatTo
            (OutputPath.IO "build/A.fs")
            defaultSettings
            (FormatCommandResult.Completed [| FormatResult.IgnoredFile "A.fs" |])

    log.Information |> shouldEqual [ "- A.fs was ignored by .fantomasignore." ]

[<Test>]
let ``a check reports invalid output the same way a format run does`` () =
    // The two commands had a copy each of the match that asks a failure to describe itself, and the
    // check copy knew about two of the three: it printed the whole explanation after `could not be
    // checked:` where a format run printed the block.
    let code, log =
        reportCheck (
            CheckCommandResult.Completed(
                [],
                {
                    Errors = [ "A.fs", InvalidCodeException(rejectedOutput, rejection) ]
                    Formatted = []
                    Unchanged = []
                }
            )
        )

    code |> shouldEqual 1

    let report: string = log.Error |> List.exactlyOne

    report.Split('\n')
    |> Array.head
    |> shouldEqual "x A.fs could not be formatted by Fantomas:"

    report |> shouldContainText "your file is unchanged"
    report |> shouldContainText "error FS0583: Unmatched '('"

[<Test>]
let ``a check that looked at no file says so rather than passing in silence`` () =
    // Silence is how a check says every file is already formatted, so a run that looked at nothing
    // cannot also be silent: a bad glob would otherwise read as a green build forever.
    let code, log =
        reportCheck (
            CheckCommandResult.Completed(
                [],
                {
                    Errors = []
                    Formatted = []
                    Unchanged = []
                }
            )
        )

    code |> shouldEqual 0
    log.Information |> shouldBeEmpty
    log.Warning |> shouldEqual [ "No F# files found in src." ]

[<Test>]
let ``a check whose every file was ignored says so rather than passing in silence`` () =
    let code, log =
        reportCheck (
            CheckCommandResult.Completed(
                [ "A.fs"; "B.fs" ],
                {
                    Errors = []
                    Formatted = []
                    Unchanged = []
                }
            )
        )

    code |> shouldEqual 0
    log.Information |> shouldBeEmpty

    log.Warning
    |> shouldEqual [ "All 2 F# files in src were ignored by .fantomasignore." ]

[<Test>]
let ``a check that found only a failure still counts what it looked at`` () =
    // The counts used to be printed only when something needed formatting, so a check whose one
    // finding was a file that would not parse ended without saying how many it had looked at.
    let code, log =
        reportCheck (
            CheckCommandResult.Completed(
                [],
                {
                    Errors = [ "A.fs", Exception "boom" ]
                    Formatted = []
                    Unchanged = [ "B.fs" ]
                }
            )
        )

    code |> shouldEqual 1
    // No fix command: there is no formatting to suggest for a file that would not parse.
    log.Information |> shouldEqual [ ""; "1 file already formatted, 1 errored." ]

[<Test>]
let ``no summary counts what an ignore file skipped`` () =
    // A pattern naming a file can be counted and a pattern naming a folder cannot, because the
    // folder is never opened, and a number right about the first and blind to the second reads as
    // though it covered both. Neither command puts it among the counts; both name it at detailed
    // verbosity, where each is exact.
    let code, log =
        reportCheck (
            CheckCommandResult.Completed(
                [ "Skip.fs" ],
                {
                    Errors = []
                    Formatted = [ "A.fs" ]
                    Unchanged = [ "B.fs" ]
                }
            )
        )

    code |> shouldEqual 99

    log.Information
    |> shouldEqual
        [
            "! A.fs needs formatting."
            ""
            "1 file needs formatting, 1 already formatted. Run dotnet fantomas src to format it."
        ]

[<Test>]
let ``a check names the file it skipped at detailed verbosity`` () =
    let _, log =
        reportCheck (
            CheckCommandResult.Completed(
                [ "Skip.fs" ],
                {
                    Errors = []
                    Formatted = [ "A.fs" ]
                    Unchanged = []
                }
            )
        )

    log.Debug |> shouldEqual [ "'Skip.fs' was ignored" ]

// Every path on the command line is accounted for somewhere: a folder by the counts it produces, a
// file by a count it is part of or by a line of its own. Skipped is the one state no count carries,
// so for a file somebody typed the line is the only place left, and it used to appear only when that
// file was the whole run.
[<Test>]
let ``a skipped file the caller named is said out loud beside other paths`` () =
    let log: CollectedLog =
        reportFormatOf
            (InputPath.Multiple([ "A.fs"; "Skip.fs" ], []))
            (FormatCommandResult.Completed [| FormatResult.Unchanged "A.fs"; FormatResult.IgnoredFile "Skip.fs" |])

    log.Information
    |> shouldEqual [ "- Skip.fs was ignored by .fantomasignore."; ""; "1 file unchanged." ]

[<Test>]
let ``a skipped file the walk turned up is named only at detailed verbosity`` () =
    // Listing these would put a vendored checkout on the screen, and no count can carry them.
    let log: CollectedLog =
        reportFormatOf
            (InputPath.Folder "src")
            (FormatCommandResult.Completed
                [| FormatResult.Unchanged "src/A.fs"; FormatResult.IgnoredFile "src/Skip.fs" |])

    log.Information |> shouldEqual [ "1 file unchanged." ]
    log.Debug |> shouldContain "'src/Skip.fs' was ignored"

[<Test>]
let ``a check says a skipped file the caller named out loud beside other paths`` () =
    let log: CollectedLog =
        reportCheckOf
            (InputPath.Multiple([ "A.fs"; "Skip.fs" ], []))
            (CheckCommandResult.Completed(
                [ "Skip.fs" ],
                {
                    Errors = []
                    Formatted = [ "A.fs" ]
                    Unchanged = []
                }
            ))

    log.Information
    |> shouldEqual
        [
            "! A.fs needs formatting."
            "- Skip.fs was ignored by .fantomasignore."
            ""
            "1 file needs formatting. Run dotnet fantomas A.fs Skip.fs to format it."
        ]

[<Test>]
let ``a skipped file the caller named is not also said at detailed verbosity`` () =
    // The sentence and the debug line are decided in one place, so one event cannot get two
    // spellings the way `unchanged` once did.
    let log: CollectedLog =
        reportFormatOf
            (InputPath.Multiple([ "A.fs"; "Skip.fs" ], []))
            (FormatCommandResult.Completed [| FormatResult.Unchanged "A.fs"; FormatResult.IgnoredFile "Skip.fs" |])

    log.Debug |> shouldEqual [ "'A.fs' was unchanged" ]

[<Test>]
let ``a check that found nothing says nothing`` () =
    let code, log =
        reportCheck (
            CheckCommandResult.Completed(
                [],
                {
                    Errors = []
                    Formatted = []
                    Unchanged = [ "A.fs"; "B.fs" ]
                }
            )
        )

    code |> shouldEqual 0
    log.Information |> shouldBeEmpty
    log.Warning |> shouldBeEmpty

[<Test>]
let ``the files a run reports are ordered by path, whatever order they came back in`` () =
    // A folder walk hands files over in whatever order the file system chose, which differs between
    // two machines running the same command. The JSON report already sorts; so does this one.
    let _, log =
        reportFormat
            defaultSettings
            (FormatCommandResult.Completed
                [|
                    FormatResult.Formatted("src/C.fs", "")
                    FormatResult.Formatted("src/A.fs", "")
                    FormatResult.Formatted("src/B.fs", "")
                |])

    log.Information
    |> shouldEqual
        [
            "+ src/A.fs was formatted."
            "+ src/B.fs was formatted."
            "+ src/C.fs was formatted."
            ""
            "3 files formatted."
        ]

[<Test>]
let ``a profile of one file says file and line rather than files and lines`` () =
    let recorded: RecordedRun = run ()

    let result: Fantomas.ProfileCommand.ProfileCommandResult =
        Fantomas.ProfileCommand.ProfileCommandResult.Completed
            {
                Timings =
                    [
                        {
                            File = "A.fs"
                            LineCount = 1
                            DefineCombinations = 1
                            TimeTaken = TimeSpan.FromMilliseconds 3.0
                        }
                    ]
                Ignored = []
                Errors = []
                Elapsed = TimeSpan.FromMilliseconds 4.0
            }

    reportProfileCommand recorded.Environment defaultSettings inputFolder result
    |> shouldEqual 0

    let log: CollectedLog = recorded.Log()

    log.Information |> List.head |> shouldContainText "Formatted 1 file serially"

    log.Information
    |> List.exists (fun line -> line.Contains "1 line ")
    |> shouldEqual true

    log.Information
    |> List.exists (fun line -> line.Contains "1 lines")
    |> shouldEqual false

[<Test>]
let ``a summary leaves out the states that did not happen`` () =
    summaryLine plainTheme [ 2, "formatted"; 0, "errored"; 30, "unchanged" ]
    |> shouldEqual "2 files formatted, 30 unchanged."

[<Test>]
let ``one failure among several files still exits 1`` () =
    let code, log =
        reportFormat
            defaultSettings
            (FormatCommandResult.Completed
                [|
                    FormatResult.Formatted("A.fs", "")
                    FormatResult.Error("B.fs", Exception "boom")
                |])

    code |> shouldEqual 1
    log.Error |> shouldEqual [ "x B.fs could not be formatted: boom" ]

[<Test>]
let ``a check of an unusable input path exits 1`` () =
    let code, log =
        reportCheck (CheckCommandResult.InvalidInput(InputProblem.NotFound "nope.fs"))

    code |> shouldEqual 1

    log.Error |> shouldEqual [ "Input path 'nope.fs' not found." ]

[<Test>]
let ``a check that failed outright is reported and exits 1`` () =
    let code, log =
        reportCheck (CheckCommandResult.Failed(Exception "the ignore file makes no sense"))

    code |> shouldEqual 1
    log.Error |> shouldEqual [ "the ignore file makes no sense" ]

[<Test>]
let ``a check with nothing to do says nothing and exits 0`` () =
    let code, log =
        reportCheck (
            CheckCommandResult.Completed(
                [],
                {
                    Errors = []
                    Formatted = []
                    Unchanged = []
                }
            )
        )

    // A question that found nothing has nothing to report. The exit code is the answer, and a
    // caller testing for empty output still gets it.
    code |> shouldEqual 0
    log.Information |> shouldBeEmpty
    log.Error |> shouldBeEmpty

[<Test>]
let ``a check names the single file it was given and then ignored`` () =
    let code, log =
        reportCheck (
            CheckCommandResult.Completed(
                [ "A.fs" ],
                {
                    Errors = []
                    Formatted = []
                    Unchanged = []
                }
            )
        )

    // Silence here reads as "already formatted" when nothing was looked at at all.
    code |> shouldEqual 0

    log.Information |> shouldEqual [ "- A.fs was ignored by .fantomasignore." ]

[<Test>]
let ``a check that found files needing formatting exits 99`` () =
    let code, log =
        reportCheck (
            CheckCommandResult.Completed(
                [],
                {
                    Errors = []
                    Formatted = [ "A.fs"; "B.fs" ]
                    Unchanged = []
                }
            )
        )

    code |> shouldEqual 99

    log.Information
    |> shouldEqual
        [
            "! A.fs needs formatting."
            "! B.fs needs formatting."
            ""
            "2 files need formatting. Run dotnet fantomas src to format them."
        ]

[<Test>]
let ``a check that could not format a file exits 1 rather than 99`` () =
    let code, log =
        reportCheck (
            CheckCommandResult.Completed(
                [],
                {
                    Errors = [ "A.fs", Exception "boom" ]
                    Formatted = []
                    Unchanged = []
                }
            )
        )

    code |> shouldEqual 1
    log.Error |> List.exactlyOne |> shouldContainText "A.fs could not be checked"

    // The file is named once, under the heading that is true of it. Telling the reader to run a
    // formatter that has already failed on it is what the old report did.
    log.Information |> shouldBeEmpty

// ---- the doctor report ----

let private diagnosed (report: Fantomas.DoctorCommand.DoctorReport) : string =
    let recorded: RecordedRun = run ()

    reportDoctorCommand
        recorded.Environment
        defaultSettings
        (Fantomas.DoctorCommand.DoctorCommandResult.Completed report)
    |> ignore

    recorded.Log().Information |> String.concat "\n"

/// A file that came through every step with nothing wrong with it, for a test to break one step of.
let private healthy: Fantomas.DoctorCommand.DoctorReport =
    {
        File =
            Fantomas.DoctorCommand.FileStep.Candidate
                {
                    Path = "/repo/A.fs"
                    Kind = Fantomas.DoctorCommand.FileKind.Implementation
                    LineCount = 20
                    UnreachableUnder = None
                }
        Ignore = Some Fantomas.DoctorCommand.IgnoreStep.NoIgnoreFile
        Settings = Some(Fantomas.EditorConfig.withoutEditorConfig FormatConfig.Default)
        Format =
            Some(Fantomas.DoctorCommand.FormatStep.Produced("let a = 1\n", Fantomas.DoctorCommand.FormatChange.Nothing))
        Validity = Some Fantomas.DoctorCommand.ValidityStep.Valid
        Idempotency = Some Fantomas.DoctorCommand.IdempotencyStep.Idempotent
    }

[<Test>]
let ``the doctor report names every step it reached, in the order Fantomas does them`` () =
    let written: string = diagnosed healthy

    let labels: string list =
        written.Split('\n')
        |> Array.toList
        |> List.choose (fun (line: string) ->
            [ "File"; "Ignore"; "Settings"; "Format"; "Valid"; "Idempotent" ]
            |> List.tryFind (fun (label: string) -> line.Contains(" " + label + " "))
        )

    labels
    |> shouldEqual [ "File"; "Ignore"; "Settings"; "Format"; "Valid"; "Idempotent" ]

[<Test>]
let ``a step the walk never reached is named as not looked at, and why`` () =
    // Five muted rows saying nothing happened is not a report of an ignored file, and leaving them
    // out without a word leaves the reader to notice the absence.
    let ignored: Fantomas.DoctorCommand.DoctorReport =
        { healthy with
            Ignore = Some(Fantomas.DoctorCommand.IgnoreStep.Governed("/repo/.fantomasignore", true, []))
            Settings = None
            Format = None
            Validity = None
            Idempotency = None
        }

    let written: string = diagnosed ignored

    written
    |> shouldContainText "Settings, Format, Valid and Idempotent were not looked at"

    written
    |> shouldContainText "does not format a file its .fantomasignore matches"

[<Test>]
let ``the whole report is on standard out, whatever any step came to`` () =
    // A trace of one file through steps in order is a block, and a step that lands on the other
    // stream arrives out of order in a terminal and goes missing entirely from a redirected one.
    let recorded: RecordedRun = run ()

    let broken: Fantomas.DoctorCommand.DoctorReport =
        { healthy with
            Format = Some(Fantomas.DoctorCommand.FormatStep.Failed(exn "could not be read"))
            Validity = None
            Idempotency = None
        }

    let code: int =
        reportDoctorCommand
            recorded.Environment
            defaultSettings
            (Fantomas.DoctorCommand.DoctorCommandResult.Completed broken)

    let log: CollectedLog = recorded.Log()

    code |> shouldEqual 1
    log.Error |> shouldBeEmpty
    String.concat "\n" log.Information |> shouldContainText "could not be read"

[<Test>]
let ``a folder is refused with the command that would answer the question instead`` () =
    let recorded: RecordedRun = run ()

    let code: int =
        reportDoctorCommand
            recorded.Environment
            defaultSettings
            (Fantomas.DoctorCommand.DoctorCommandResult.NotOneFile(InputPath.Folder "src"))

    let log: CollectedLog = recorded.Log()

    code |> shouldEqual 1
    String.concat "\n" log.Error |> shouldContainText "doctor reports on one file"
    String.concat "\n" log.Error |> shouldContainText "check src"

[<Test>]
let ``the settings line names the .editorconfig it means, absolutely`` () =
    // `.editorconfig` on its own is the one thing somebody reading this report cannot go and open,
    // and every other path it prints is absolute.
    let resolved: Fantomas.EditorConfig.ResolvedConfig =
        let plain: Fantomas.EditorConfig.ResolvedConfig =
            Fantomas.EditorConfig.withoutEditorConfig FormatConfig.Default

        { plain with
            // A chain of two, where only the further one sets anything Fantomas reads.
            EditorConfigFiles = [ "/repo/.editorconfig"; "/repo/src/.editorconfig" ]
            Settings =
                plain.Settings
                |> List.map (fun (setting: Fantomas.EditorConfig.ResolvedSetting) ->
                    if setting.Setting = "max_line_length" then
                        { setting with
                            SetBy = Some "/repo/.editorconfig"
                        }
                    else
                        setting
                )
        }

    let written: string =
        diagnosed
            { healthy with
                Settings = Some resolved
            }

    written |> shouldContainText "come from /repo/.editorconfig,"

    // The nearer file is in the chain and set nothing Fantomas reads, so it is not named as having
    // contributed a setting. Naming it would send somebody to edit the wrong file.
    written |> shouldNotContainText "/repo/src/.editorconfig"

[<Test>]
let ``every setting the file will be formatted with is listed`` () =
    // The step is answering what the file will be formatted with, and that is not answered by a
    // list with most of it left out.
    let written: string = diagnosed healthy

    for setting in Fantomas.EditorConfig.supportedSettings do
        written |> shouldContainText setting

[<Test>]
let ``the settings an .editorconfig set are set apart from the defaults by a blank line`` () =
    let fromEditorConfig: Fantomas.EditorConfig.ResolvedConfig =
        let resolved: Fantomas.EditorConfig.ResolvedConfig =
            Fantomas.EditorConfig.withoutEditorConfig FormatConfig.Default

        { resolved with
            EditorConfigFiles = [ "/repo/.editorconfig" ]
            Settings =
                resolved.Settings
                |> List.map (fun (setting: Fantomas.EditorConfig.ResolvedSetting) ->
                    if setting.Setting = "max_line_length" then
                        { setting with
                            SetBy = Some "/repo/.editorconfig"
                        }
                    else
                        setting
                )
        }

    let written: string list =
        let recorded: RecordedRun = run ()

        reportDoctorCommand
            recorded.Environment
            defaultSettings
            (Fantomas.DoctorCommand.DoctorCommandResult.Completed
                { healthy with
                    Settings = Some fromEditorConfig
                })
        |> ignore

        recorded.Log().Information

    let indexOf (text: string) : int =
        written |> List.findIndex (fun (line: string) -> line.Contains text)

    let blankAfterTheEditorConfigOne: int = indexOf "max_line_length" + 1

    written.[blankAfterTheEditorConfigOne] |> shouldEqual ""

    // The group below the blank line is what nothing set, and it is still there.
    indexOf "fsharp_max_record_width"
    |> shouldBeGreaterThan blankAfterTheEditorConfigOne

// ---- one case per row the doctor report can draw ----
//
// The wording is what this command is, so every branch of it is rendered here and read back. They
// are pure functions of a `DoctorReport`, so a step no run can be made to produce on demand is
// still a step whose sentence can be checked.

let private candidate (kind: Fantomas.DoctorCommand.FileKind) (under: string option) : Fantomas.DoctorCommand.FileStep =
    Fantomas.DoctorCommand.FileStep.Candidate
        {
            Path = "/repo/A.fs"
            Kind = kind
            LineCount = 20
            UnreachableUnder = under
        }

let private matched (line: int) (pattern: string) (negated: bool) : Fantomas.IgnoreMatch =
    {
        LineNumber = line
        Pattern = pattern
        Negated = negated
    }

[<Test>]
let ``each way the file step can end has a sentence of its own`` () =
    let says (step: Fantomas.DoctorCommand.FileStep) : string = diagnosed { healthy with File = step }

    says (Fantomas.DoctorCommand.FileStep.NotFound "/repo/A.fs")
    |> shouldContainText "There is no file at this path."

    says (Fantomas.DoctorCommand.FileStep.NotFSharp "/repo/A.md")
    |> shouldContainText "Found on disk, but not a file Fantomas formats."

    says (candidate Fantomas.DoctorCommand.FileKind.Implementation None)
    |> shouldContainText "an implementation file of 20 lines"

    says (candidate Fantomas.DoctorCommand.FileKind.Signature None)
    |> shouldContainText "a signature file of 20 lines"

    says (candidate Fantomas.DoctorCommand.FileKind.Script None)
    |> shouldContainText "a script file of 20 lines"

[<Test>]
let ``a file under a folder a walk will not open says which folder`` () =
    let written: string =
        diagnosed
            { healthy with
                File = candidate Fantomas.DoctorCommand.FileKind.Implementation (Some "/repo/src/obj")
            }

    written |> shouldContainText "It sits under /repo/src/obj"
    written |> shouldContainText "does not reach this file"

[<Test>]
let ``each way the ignore step can end has a sentence of its own`` () =
    let says (step: Fantomas.DoctorCommand.IgnoreStep) : string =
        diagnosed { healthy with Ignore = Some step }

    says Fantomas.DoctorCommand.IgnoreStep.NoIgnoreFile
    |> shouldContainText "No .fantomasignore at or above this file."

    says (Fantomas.DoctorCommand.IgnoreStep.Governed("/repo/.fantomasignore", false, []))
    |> shouldContainText "Governed by /repo/.fantomasignore, and no pattern in it matches."

    says (Fantomas.DoctorCommand.IgnoreStep.Governed("/repo/.fantomasignore", true, [ matched 4 "obj/" false ]))
    |> shouldContainText "Matched by /repo/.fantomasignore, line 4: obj/"

    says (
        Fantomas.DoctorCommand.IgnoreStep.Governed(
            "/repo/.fantomasignore",
            false,
            [ matched 1 "*.fs" false; matched 2 "!A.fs" true ]
        )
    )
    |> shouldContainText "line 2 of /repo/.fantomasignore, !A.fs, takes it back out"

[<Test>]
let ``a file matched by an ignore file that names no pattern says so rather than guessing`` () =
    // The library said yes and no pattern of the file says so on its own. The verdict is what
    // decides what happens to the file, so the verdict is what is reported.
    diagnosed
        { healthy with
            Ignore = Some(Fantomas.DoctorCommand.IgnoreStep.Governed("/repo/.fantomasignore", true, []))
        }
    |> shouldContainText "Which pattern matched could not be worked out."

[<Test>]
let ``several matching patterns are read out, with which of them decided`` () =
    let written: string =
        diagnosed
            { healthy with
                Ignore =
                    Some(
                        Fantomas.DoctorCommand.IgnoreStep.Governed(
                            "/repo/.fantomasignore",
                            true,
                            [ matched 1 "*.fs" false; matched 3 "A.fs" false ]
                        )
                    )
            }

    written |> shouldContainText "line 1: *.fs"
    written |> shouldContainText "line 3: A.fs"
    written |> shouldContainText "The last of these decides."

[<Test>]
let ``the difference from gitignore is said to whoever asks for detail`` () =
    let ignoreStep: Fantomas.DoctorCommand.IgnoreStep =
        Fantomas.DoctorCommand.IgnoreStep.Governed("/repo/.fantomasignore", false, [])

    let recorded: RecordedRun = run ()

    reportDoctorCommand
        recorded.Environment
        { defaultSettings with
            Verbosity = VerbosityLevel.Detailed
        }
        (Fantomas.DoctorCommand.DoctorCommandResult.Completed
            { healthy with
                Ignore = Some ignoreStep
            })
    |> ignore

    let detailed: string = recorded.Log().Information |> String.concat "\n"

    detailed |> shouldContainText "does not merge in the ones above it"

    // Not on every run: it is a difference worth knowing and not one worth repeating.
    diagnosed
        { healthy with
            Ignore = Some ignoreStep
        }
    |> shouldNotContainText "does not merge in the ones above it"

[<Test>]
let ``an .editorconfig that sets nothing Fantomas reads is named as having set nothing`` () =
    diagnosed
        { healthy with
            Settings =
                Some
                    { Fantomas.EditorConfig.withoutEditorConfig FormatConfig.Default with
                        EditorConfigFiles = [ "/repo/.editorconfig" ]
                    }
        }
    |> shouldContainText "/repo/.editorconfig sets nothing Fantomas reads"

[<Test>]
let ``a setting Fantomas cannot use is reported under the settings that apply`` () =
    let written: string =
        diagnosed
            { healthy with
                Settings =
                    Some
                        { Fantomas.EditorConfig.withoutEditorConfig FormatConfig.Default with
                            EditorConfigFiles = [ "/repo/.editorconfig" ]
                            Problems =
                                [
                                    Fantomas.EditorConfig.EditorConfigProblem.UnknownSetting "fsharp_nope"
                                    Fantomas.EditorConfig.EditorConfigProblem.UnrecognizedValue(
                                        "fsharp_max_record_width",
                                        "banana"
                                    )
                                ]
                        }
            }

    written |> shouldContainText "'fsharp_nope' is not a Fantomas setting."
    written |> shouldContainText "does not accept the value 'banana'"

[<Test>]
let ``each way the format step can end has a sentence of its own`` () =
    let says (step: Fantomas.DoctorCommand.FormatStep) : string =
        diagnosed
            { healthy with
                Format = Some step
                Validity = None
                Idempotency = None
            }

    says (Fantomas.DoctorCommand.FormatStep.Produced("", Fantomas.DoctorCommand.FormatChange.Nothing))
    |> shouldContainText "Already formatted. Nothing would change."

    // The file keeps its length, so where it parts from the result is the whole answer.
    says (Fantomas.DoctorCommand.FormatStep.Produced("", Fantomas.DoctorCommand.FormatChange.Reformatted(12, 20)))
    |> shouldContainText "Not formatted: the first change is at line 12."

    // It does not, so the lengths are worth saying as well.
    says (Fantomas.DoctorCommand.FormatStep.Produced("", Fantomas.DoctorCommand.FormatChange.Reformatted(1, 24)))
    |> shouldContainText "the first change is at line 1, and the file would go from 20 lines to 24."

    // Not a count of nought, which reads as nothing to do. Every line is right and the file would
    // still be rewritten, so it gets a sentence rather than a number.
    says (Fantomas.DoctorCommand.FormatStep.Produced("", Fantomas.DoctorCommand.FormatChange.LineEndingsOnly))
    |> shouldContainText "the line endings are not, so the whole file would be rewritten"

    says (Fantomas.DoctorCommand.FormatStep.Failed(exn "Access to the path is denied"))
    |> shouldContainText "Formatting failed: Access to the path is denied"

[<Test>]
let ``a failure with nothing to say for itself still says that it happened`` () =
    diagnosed
        { healthy with
            Format = Some(Fantomas.DoctorCommand.FormatStep.Failed(exn ""))
            Validity = None
            Idempotency = None
        }
    |> shouldContainText "Formatting failed."

[<Test>]
let ``output Fantomas will not accept is reported with what the parser said about it`` () =
    let written: string =
        diagnosed
            { healthy with
                Format =
                    Some(
                        Fantomas.DoctorCommand.FormatStep.Produced(
                            rejectedOutput,
                            Fantomas.DoctorCommand.FormatChange.Reformatted(3, 3)
                        )
                    )
                Validity = Some(Fantomas.DoctorCommand.ValidityStep.Invalid rejection)
                Idempotency = None
            }

    written |> shouldContainText "will not accept what it produced"
    written |> shouldContainText "a bug in Fantomas"
    written |> shouldContainText "error FS0583: Unmatched '('"
    written |> shouldContainText "Idempotent was not looked at"

[<Test>]
let ``each way the idempotency step can end has a sentence of its own`` () =
    let says (step: Fantomas.DoctorCommand.IdempotencyStep) : string =
        diagnosed { healthy with Idempotency = Some step }

    says Fantomas.DoctorCommand.IdempotencyStep.Idempotent
    |> shouldContainText "Formatting the result again changes nothing."

    says (Fantomas.DoctorCommand.IdempotencyStep.Failed(exn "the second pass fell over"))
    |> shouldContainText "Formatting the result again failed: the second pass fell over"

    let disagreed: string =
        says (Fantomas.DoctorCommand.IdempotencyStep.NotIdempotent(7, "let a = 1", "let a =  1"))

    disagreed |> shouldContainText "changes it, first at line 7"
    disagreed |> shouldContainText "after one pass:   let a = 1"
    disagreed |> shouldContainText "after two passes: let a =  1"

[<Test>]
let ``a run that fell over says what went wrong, on standard error`` () =
    let recorded: RecordedRun = run ()

    let code: int =
        reportDoctorCommand
            recorded.Environment
            defaultSettings
            (Fantomas.DoctorCommand.DoctorCommandResult.Failed(exn "the disk went away"))

    code |> shouldEqual 1

    String.concat "\n" (recorded.Log().Error)
    |> shouldContainText "the disk went away"

[<Test>]
let ``several paths are refused by saying how many were given`` () =
    let recorded: RecordedRun = run ()

    reportDoctorCommand
        recorded.Environment
        defaultSettings
        (Fantomas.DoctorCommand.DoctorCommandResult.NotOneFile(InputPath.Multiple([ "A.fs"; "B.fs" ], [ "src" ])))
    |> ignore

    String.concat "\n" (recorded.Log().Error)
    |> shouldContainText "3 paths were given"

/// A report that stopped at the file step, which is the shape a real run produces for a path it
/// cannot look at: every step below it was never reached.
let private stoppedAtTheFile (step: Fantomas.DoctorCommand.FileStep) : Fantomas.DoctorCommand.DoctorReport =
    {
        File = step
        Ignore = None
        Settings = None
        Format = None
        Validity = None
        Idempotency = None
    }

[<Test>]
let ``a path that is not there says nothing below it was looked at, and why`` () =
    let written: string =
        diagnosed (stoppedAtTheFile (Fantomas.DoctorCommand.FileStep.NotFound "/repo/A.fs"))

    written
    |> shouldContainText "Ignore, Settings, Format, Valid and Idempotent were not looked at"

    written |> shouldContainText "there is no file here to put through them"

[<Test>]
let ``a file Fantomas does not format says so as the reason the rest was skipped`` () =
    diagnosed (stoppedAtTheFile (Fantomas.DoctorCommand.FileStep.NotFSharp "/repo/A.md"))
    |> shouldContainText "Fantomas does not format this kind of file"

[<Test>]
let ``a file that will not parse is reported with the parser's own diagnostics and a snippet`` () =
    // The failure describes itself below the table, at full width, because a snippet with a caret
    // under it is not something that survives being indented into a column. The lines it draws come
    // from the file on disk, so this one is a real file and a real file system.
    use fileFixture = new TemporaryFileCodeSample(rejectedOutput)

    let recorded: RecordedRun =
        recordingEnvironment (System.IO.Abstractions.FileSystem()) None

    reportDoctorCommand
        recorded.Environment
        defaultSettings
        (Fantomas.DoctorCommand.DoctorCommandResult.Completed
            { stoppedAtTheFile (
                  Fantomas.DoctorCommand.FileStep.Candidate
                      {
                          Path = fileFixture.Filename
                          Kind = Fantomas.DoctorCommand.FileKind.Implementation
                          LineCount = 3
                          UnreachableUnder = None
                      }
              ) with
                Format = Some(Fantomas.DoctorCommand.FormatStep.Failed(ParseException rejection))
            })
    |> ignore

    let written: string = recorded.Log().Information |> String.concat "\n"

    written |> shouldContainText "error FS0583: Unmatched '('"
    // Read back off the file, which is the one thing here that needs the path to still resolve.
    written |> shouldContainText "let a = (1"

[<Test>]
let ``a single path that is somehow refused still says what the command takes`` () =
    // `runDoctorCommand` only refuses a folder and a list, so this is the way through that keeps
    // the match total rather than a shape a run can produce.
    let recorded: RecordedRun = run ()

    reportDoctorCommand
        recorded.Environment
        defaultSettings
        (Fantomas.DoctorCommand.DoctorCommandResult.NotOneFile(InputPath.File "A.fs"))
    |> ignore

    String.concat "\n" (recorded.Log().Error)
    |> shouldContainText "doctor reports on one file."
