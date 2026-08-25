module Fantomas.Tests.ReportTests

open System
open System.IO.Abstractions.TestingHelpers
open NUnit.Framework
open FsUnitTyped
open Fantomas.Core
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

    log.Error |> shouldEqual [ "x sub/A.fs could not be formatted." ]

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

    log.Information
    |> shouldEqual
        [
            "! sub/A.fs needs formatting."
            ""
            "1 needs formatting. Run dotnet fantomas src to format it."
        ]

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
    log.Error |> shouldEqual [ "x A.fs could not be formatted." ]

[<Test>]
let ``code that came out invalid is reported as a failure and exits 1`` () =
    let code, log =
        reportFormat defaultSettings (FormatCommandResult.Completed [| FormatResult.InvalidCode("A.fs", "let a =") |])

    code |> shouldEqual 1

    log.Error
    |> shouldEqual [ "x A.fs could not be formatted: Formatting A.fs leads to invalid F# code" ]

[<Test>]
let ``a detailed run reports the whole exception rather than a line`` () =
    let settings: Fantomas.Cli.CliSettings =
        { defaultSettings with
            Verbosity = VerbosityLevel.Detailed
        }

    let code, log =
        reportFormat settings (FormatCommandResult.Completed [| FormatResult.Error("A.fs", Exception "boom") |])

    code |> shouldEqual 1
    log.Error |> List.exactlyOne |> shouldContainText "System.Exception: boom"

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
    |> shouldEqual [ "+ A.fs was formatted."; ""; "1 formatted, 1 unchanged, 1 ignored." ]

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
let ``a summary leaves out the states that did not happen`` () =
    summaryLine plainTheme [ 2, "formatted"; 0, "errored"; 30, "unchanged" ]
    |> shouldEqual "2 formatted, 30 unchanged."

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
    log.Error |> shouldEqual [ "x B.fs could not be formatted." ]

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
            "2 need formatting. Run dotnet fantomas src to format them."
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
