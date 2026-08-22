module Fantomas.Tests.ReportTests

open System
open System.IO.Abstractions.TestingHelpers
open NUnit.Framework
open FsUnitTyped
open Fantomas.Core
open Fantomas.CommandResult
open Fantomas.Logging
open Fantomas.Report
open Fantomas.Tests.TestHelpers

let private run () : RecordedRun =
    recordingEnvironment (MockFileSystem()) None

let private reportFormat (settings: Fantomas.Cli.CliSettings) (result: FormatCommandResult) =
    let recorded: RecordedRun = run ()
    let code: int = reportFormatCommand recorded.Environment settings result
    code, recorded.Log(), recorded.Drawn()

let private reportCheck (result: CheckCommandResult) =
    let recorded: RecordedRun = run ()
    let code: int = reportCheckCommand recorded.Environment result
    code, recorded.Log()

[<Test>]
let ``every way the input paths can fail has its own wording`` () =
    [ InputProblem.UnsupportedFileType "A.md"
      InputProblem.NotFound "A.fs"
      InputProblem.NoPathGiven
      InputProblem.MultiplePathsWithOut ]
    |> List.map describeInputProblem
    |> shouldEqual
        [ "Input path 'A.md' is an unsupported file type."
          "Input path 'A.fs' not found."
          "No input path provided. Call with --help for usage information."
          "Multiple input files are not supported with the --out flag." ]

[<Test>]
let ``an unusable input path is reported on error and exits 1`` () =
    let code, log, _ =
        reportFormat defaultSettings (FormatCommandResult.InvalidInput(InputProblem.NotFound "A.fs"))

    code |> shouldEqual 1
    log.Error |> shouldEqual [ "Input path 'A.fs' not found." ]
    log.Information |> shouldBeEmpty

[<Test>]
let ``a failure no single file can be blamed for is reported and exits 1`` () =
    let code, log, _ =
        reportFormat defaultSettings (FormatCommandResult.Failed(Exception "the disk went away"))

    code |> shouldEqual 1
    log.Error |> shouldEqual [ "the disk went away" ]

[<Test>]
let ``a single formatted file is reported as a sentence naming it`` () =
    let code, log, drawn =
        reportFormat
            defaultSettings
            (FormatCommandResult.Completed [| FormatResult.Formatted("A.fs", "let a = 1", None) |])

    code |> shouldEqual 0
    log.Information |> shouldEqual [ "A.fs was formatted." ]
    // A table of one row would say less than the sentence does, so none is drawn.
    drawn |> shouldEqual ""

[<Test>]
let ``a file is reported by the path it was given, not by its name alone`` () =
    // Reducing the path to its file name is the regression this guards: `sub/A.fs` has to come
    // back as `sub/A.fs`, whatever spelling the caller used.
    let path: string = "sub/A.fs"

    let formatted, log, _ =
        reportFormat defaultSettings (FormatCommandResult.Completed [| FormatResult.Formatted(path, "", None) |])

    let _, unchangedLog, _ =
        reportFormat defaultSettings (FormatCommandResult.Completed [| FormatResult.Unchanged(path, None) |])

    let _, ignoredLog, _ =
        reportFormat defaultSettings (FormatCommandResult.Completed [| FormatResult.IgnoredFile path |])

    formatted |> shouldEqual 0
    log.Information |> shouldEqual [ "sub/A.fs was formatted." ]
    unchangedLog.Information |> shouldEqual [ "sub/A.fs was unchanged." ]
    ignoredLog.Information |> shouldEqual [ "sub/A.fs was ignored." ]

[<Test>]
let ``a file that failed is reported by the path it was given`` () =
    let _, log, _ =
        reportFormat
            defaultSettings
            (FormatCommandResult.Completed [| FormatResult.Error("sub/A.fs", Exception "boom") |])

    log.Error |> shouldEqual [ "Failed to format file: sub/A.fs" ]

[<Test>]
let ``a check names the files it found by the path it was given`` () =
    let _, log =
        reportCheck (
            CheckCommandResult.Completed(
                [],
                { Errors = []
                  Formatted = [ "sub/A.fs" ] }
            )
        )

    log.Information |> shouldEqual [ "sub/A.fs needs formatting" ]

[<Test>]
let ``the files a run ignored are named at detailed verbosity`` () =
    // A single result is named outright; among several, only the counts table is drawn, so this
    // debug line is the only place a folder run says which files it skipped.
    let _, log, _ =
        reportFormat
            defaultSettings
            (FormatCommandResult.Completed
                [| FormatResult.Formatted("A.fs", "", None)
                   FormatResult.IgnoredFile "sub/B.fs" |])

    log.Debug |> shouldEqual [ "'sub/B.fs' was ignored" ]

[<Test>]
let ``a single unchanged file is reported as unchanged`` () =
    let code, log, _ =
        reportFormat defaultSettings (FormatCommandResult.Completed [| FormatResult.Unchanged("A.fs", None) |])

    code |> shouldEqual 0
    log.Information |> shouldEqual [ "A.fs was unchanged." ]

[<Test>]
let ``a single ignored file is reported as ignored`` () =
    let code, log, _ =
        reportFormat defaultSettings (FormatCommandResult.Completed [| FormatResult.IgnoredFile "A.fs" |])

    code |> shouldEqual 0
    log.Information |> shouldEqual [ "A.fs was ignored." ]

[<Test>]
let ``a single file that failed is reported on error and exits 1`` () =
    let code, log, _ =
        reportFormat defaultSettings (FormatCommandResult.Completed [| FormatResult.Error("A.fs", Exception "boom") |])

    code |> shouldEqual 1
    log.Error |> shouldEqual [ "Failed to format file: A.fs" ]

[<Test>]
let ``code that came out invalid is reported as a failure and exits 1`` () =
    let code, log, _ =
        reportFormat defaultSettings (FormatCommandResult.Completed [| FormatResult.InvalidCode("A.fs", "let a =") |])

    code |> shouldEqual 1

    log.Error
    |> shouldEqual [ "Failed to format file: A.fs : Formatting A.fs leads to invalid F# code" ]

[<Test>]
let ``a detailed run reports the whole exception rather than a line`` () =
    let settings: Fantomas.Cli.CliSettings =
        { defaultSettings with
            Verbosity = VerbosityLevel.Detailed }

    let code, log, _ =
        reportFormat settings (FormatCommandResult.Completed [| FormatResult.Error("A.fs", Exception "boom") |])

    code |> shouldEqual 1
    log.Error |> List.exactlyOne |> shouldContainText "System.Exception: boom"

[<Test>]
let ``several files are reported as a table of counts`` () =
    let code, log, drawn =
        reportFormat
            defaultSettings
            (FormatCommandResult.Completed
                [| FormatResult.Formatted("A.fs", "", None)
                   FormatResult.Unchanged("B.fs", None)
                   FormatResult.IgnoredFile "C.fs" |])

    code |> shouldEqual 0
    // No sentence per file: the table is the report.
    log.Information |> shouldBeEmpty
    drawn |> shouldContainText "Formatted"
    drawn |> shouldContainText "Ignored"
    drawn |> shouldContainText "Unchanged"
    drawn |> shouldContainText "Errored"

[<Test>]
let ``one failure among several files still exits 1`` () =
    let code, log, _ =
        reportFormat
            defaultSettings
            (FormatCommandResult.Completed
                [| FormatResult.Formatted("A.fs", "", None)
                   FormatResult.Error("B.fs", Exception "boom") |])

    code |> shouldEqual 1
    log.Error |> shouldEqual [ "Failed to format file: B.fs" ]

[<Test>]
let ``profiling reports the line count and the time taken for a single file`` () =
    let settings: Fantomas.Cli.CliSettings = { defaultSettings with Profile = true }

    let profile: ProfileInfo option =
        Some
            { LineCount = 12
              TimeTaken = TimeSpan.FromSeconds 1.0 }

    let _, log, _ =
        reportFormat settings (FormatCommandResult.Completed [| FormatResult.Formatted("A.fs", "", profile) |])

    log.Information |> List.last |> shouldContainText "Line count: 12"

[<Test>]
let ``nothing is profiled unless profiling was asked for`` () =
    let profile: ProfileInfo option =
        Some
            { LineCount = 12
              TimeTaken = TimeSpan.FromSeconds 1.0 }

    let _, log, _ =
        reportFormat defaultSettings (FormatCommandResult.Completed [| FormatResult.Formatted("A.fs", "", profile) |])

    log.Information |> shouldEqual [ "A.fs was formatted." ]

[<Test>]
let ``a check of an unusable input path exits 1`` () =
    let code, log =
        reportCheck (CheckCommandResult.InvalidInput InputProblem.NoPathGiven)

    code |> shouldEqual 1

    log.Error
    |> shouldEqual [ "No input path provided. Call with --help for usage information." ]

[<Test>]
let ``a check that failed outright is reported and exits 1`` () =
    let code, log =
        reportCheck (CheckCommandResult.Failed(Exception "the ignore file makes no sense"))

    code |> shouldEqual 1
    log.Error |> shouldEqual [ "the ignore file makes no sense" ]

[<Test>]
let ``a check with nothing to do exits 0`` () =
    let code, log =
        reportCheck (CheckCommandResult.Completed([], { Errors = []; Formatted = [] }))

    code |> shouldEqual 0
    log.Debug |> shouldEqual [ "No changes required." ]

[<Test>]
let ``a check reports the files it ignored`` () =
    let code, log =
        reportCheck (CheckCommandResult.Completed([ "A.fs" ], { Errors = []; Formatted = [] }))

    code |> shouldEqual 0
    log.Debug |> shouldContain "'A.fs' was ignored"

[<Test>]
let ``a check that found files needing formatting exits 99`` () =
    let code, log =
        reportCheck (
            CheckCommandResult.Completed(
                [],
                { Errors = []
                  Formatted = [ "A.fs"; "B.fs" ] }
            )
        )

    code |> shouldEqual 99

    log.Information
    |> shouldEqual [ "A.fs needs formatting"; "B.fs needs formatting" ]

[<Test>]
let ``a check that could not format a file exits 1 rather than 99`` () =
    let code, log =
        reportCheck (
            CheckCommandResult.Completed(
                [],
                { Errors = [ "A.fs", Exception "boom" ]
                  Formatted = [ "A.fs" ] }
            )
        )

    code |> shouldEqual 1
    log.Error |> List.exactlyOne |> shouldContainText "Failed to format A.fs"
