module Fantomas.Tests.Integration.JsonTests

open System.Text.Json
open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelpers

// What the document says is settled in JsonReportTests, against the report builders. What is left
// here is what only a real process shows: that standard out carries the document and nothing else,
// and that asking for it changes no exit code.

let private document (output: string) : JsonElement = JsonDocument.Parse(output).RootElement

let private statuses (output: string) : (string * string) list =
    (document output).GetProperty("files").EnumerateArray()
    |> Seq.map (fun file -> file.GetProperty("path").GetString(), file.GetProperty("status").GetString())
    |> List.ofSeq

[<Test>]
let ``standard out carries the document and nothing else`` () =
    use fileFixture = new TemporaryFileCodeSample("let a =   0")

    let {
            ExitCode = exitCode
            Output = output
            Error = error
        } =
        runFantomasTool [ "--json"; fileFixture.Filename ]

    exitCode |> should equal 0
    statuses output |> should equal [ fileFixture.Filename, "formatted" ]

    // The document is the whole report. The sentence a run without --json prints is not moved to
    // standard error, it is not written at all, so neither stream carries a second account.
    Assert.That(output, Does.Not.Contain "was formatted")
    error |> should equal ""

[<Test>]
let ``a parse failure is reported in the document instead of on standard error`` () =
    use fileFixture = new TemporaryFileCodeSample("module A\n\nlet a = (1 + 2\n")

    let {
            ExitCode = exitCode
            Output = output
            Error = error
        } =
        runFantomasTool [ "--json"; fileFixture.Filename ]

    exitCode |> should equal 1
    error |> should equal ""

    let file: JsonElement =
        (document output).GetProperty("files").EnumerateArray() |> Seq.exactlyOne

    file.GetProperty("status").GetString() |> should equal "error"

    let diagnostic: JsonElement =
        file.GetProperty("diagnostics").EnumerateArray() |> Seq.exactlyOne

    diagnostic.GetProperty("code").GetString() |> should equal "FS0583"

    diagnostic.GetProperty("range").GetProperty("startLine").GetInt32()
    |> should equal 3

    diagnostic.GetProperty("range").GetProperty("startColumn").GetInt32()
    |> should equal 9

// The three codes --check ends with are what a pipeline branches on, and asking for a document
// instead of a table does not change what the run found.
[<Test>]
let ``--check keeps its exit codes`` () =
    use needsFormatting = new TemporaryFileCodeSample("let a =   0")

    let { ExitCode = exitCode; Output = output } =
        runFantomasTool [ "--check"; "--json"; needsFormatting.Filename ]

    exitCode |> should equal 99
    (document output).GetProperty("exitCode").GetInt32() |> should equal 99
    statuses output |> should equal [ needsFormatting.Filename, "needs-formatting" ]

// A check reports every file it looked at, the way a format run does, so a caller reads "already
// formatted" off a status rather than off a file being missing from the document.
[<Test>]
let ``a check names the files it found nothing to say about`` () =
    use alreadyFormatted = new TemporaryFileCodeSample("let a = 0\n")

    let { ExitCode = exitCode; Output = output } =
        runFantomasTool [ "--check"; "--json"; alreadyFormatted.Filename ]

    exitCode |> should equal 0
    statuses output |> should equal [ alreadyFormatted.Filename, "unchanged" ]

[<Test>]
let ``an unusable input path is reported in the document rather than only on standard error`` () =
    let { ExitCode = exitCode; Output = output } =
        runFantomasTool [ "--json"; "this-file-does-not-exist.fs" ]

    exitCode |> should equal 1

    (document output).GetProperty("error").GetString()
    |> should equal "Input path 'this-file-does-not-exist.fs' not found."

[<Test>]
let ``the help page lists the flag`` () =
    let { Output = output } = runFantomasTool [ "--help" ]
    Assert.That(output, Does.Contain "--json")
