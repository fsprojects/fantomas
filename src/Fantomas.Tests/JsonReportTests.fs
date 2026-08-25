module Fantomas.Tests.JsonReportTests

open System
open System.Text.Json
open NUnit.Framework
open FsUnitTyped
open Fantomas.Core
open Fantomas.FCS.Diagnostics
open Fantomas.FCS.Parse
open Fantomas.FCS.Text
open Fantomas.CommandResult
open Fantomas.JsonReport

/// The folder the paths in a document are relative to. A literal rather than the real one, so that
/// what these tests assert does not depend on where they are run from.
let private workingDirectory: string = "/repo"

let private parsed (result: RunReport) : JsonElement =
    JsonDocument.Parse(render result).RootElement

let private files (document: JsonElement) : JsonElement list =
    document.GetProperty("files").EnumerateArray() |> List.ofSeq

let private statusOf (file: JsonElement) : string * string =
    file.GetProperty("path").GetString(), file.GetProperty("status").GetString()

let private completed (results: FormatResult list) : JsonElement =
    FormatCommandResult.Completed(Array.ofList results)
    |> formatReport workingDirectory
    |> parsed

let private checked'
    (ignored: string list)
    (errors: (string * exn) list)
    (formatted: string list)
    (unchanged: string list)
    : JsonElement
    =
    CheckCommandResult.Completed(
        ignored,
        {
            Errors = errors
            Formatted = formatted
            Unchanged = unchanged
        }
    )
    |> checkReport workingDirectory
    |> parsed

let private unmatchedBracket: exn =
    let range: range = Range.mkRange "tmp.fsx" (Position.mkPos 3 8) (Position.mkPos 3 9)

    ParseException
        [
            {
                Severity = FSharpDiagnosticSeverity.Error
                SubCategory = "parse"
                Range = Some range
                ErrorNumber = Some 583
                Message = "Unmatched '('"
            }
        ]

[<Test>]
let ``a format run reports one entry per file, with what became of it`` () =
    let document: JsonElement =
        completed
            [
                FormatResult.Formatted("a.fs", "")
                FormatResult.Unchanged "b.fs"
                FormatResult.IgnoredFile "c.fs"
                FormatResult.Error("d.fs", Exception "the disk went away")
            ]

    files document
    |> List.map statusOf
    |> shouldEqual [ "a.fs", "formatted"; "b.fs", "unchanged"; "c.fs", "ignored"; "d.fs", "error" ]

[<Test>]
let ``the document says which command produced it and carries the schema version`` () =
    let format: JsonElement = completed []
    format.GetProperty("command").GetString() |> shouldEqual "format"
    format.GetProperty("version").GetInt32() |> shouldEqual SchemaVersion

    let check: JsonElement = checked' [] [] [] []
    check.GetProperty("command").GetString() |> shouldEqual "check"

// A caller that captures the document has the exit code in hand without also having to keep the
// one the process ended with, and the two cannot disagree because they are the same number.
// A path is reported as the run was given it, which is usually relative and is the shorter of the
// two. The folder it is relative to is said once, so a reader can still resolve it.
[<Test>]
let ``the folder the paths are relative to is carried once, not repeated per file`` () =
    let document: JsonElement = completed [ FormatResult.Formatted("src/A.fs", "") ]

    document.GetProperty("workingDirectory").GetString()
    |> shouldEqual workingDirectory

    files document |> List.map (statusOf >> fst) |> shouldEqual [ "src/A.fs" ]

[<Test>]
let ``the exit code the process ends with is in the document`` () =
    let clean: FormatCommandResult =
        FormatCommandResult.Completed [| FormatResult.Unchanged "a.fs" |]

    (parsed (formatReport workingDirectory clean)).GetProperty("exitCode").GetInt32()
    |> shouldEqual clean.ExitCode

    let failed: FormatCommandResult =
        FormatCommandResult.Completed [| FormatResult.Error("a.fs", Exception "nope") |]

    (parsed (formatReport workingDirectory failed)).GetProperty("exitCode").GetInt32()
    |> shouldEqual failed.ExitCode

[<Test>]
let ``a check that found files needing formatting reports 99 in the document`` () =
    let result: CheckCommandResult =
        CheckCommandResult.Completed(
            [],
            {
                Errors = []
                Formatted = [ "a.fs" ]
                Unchanged = []
            }
        )

    let document: JsonElement = parsed (checkReport workingDirectory result)
    document.GetProperty("exitCode").GetInt32() |> shouldEqual 99

    files document
    |> List.map statusOf
    |> shouldEqual [ "a.fs", "needs-formatting" ]

[<Test>]
let ``an unusable input path is reported as the run failing, with no files`` () =
    let document: JsonElement =
        FormatCommandResult.InvalidInput(InputProblem.NotFound "a.fs")
        |> formatReport workingDirectory
        |> parsed

    document.GetProperty("error").GetString()
    |> shouldEqual "Input path 'a.fs' not found."

    files document |> shouldBeEmpty

[<Test>]
let ``a run that reached its files carries no run level error`` () =
    let document: JsonElement = completed [ FormatResult.Unchanged "a.fs" ]
    document.GetProperty("error").ValueKind |> shouldEqual JsonValueKind.Null

[<Test>]
let ``a parse failure carries every diagnostic with the position the compiler would print`` () =
    let document: JsonElement =
        completed [ FormatResult.Error("a.fs", unmatchedBracket) ]

    let file: JsonElement = files document |> List.exactlyOne

    file.GetProperty("message").GetString()
    |> shouldEqual "a.fs could not be parsed by Fantomas"

    let diagnostic: JsonElement =
        file.GetProperty("diagnostics").EnumerateArray() |> Seq.exactlyOne

    diagnostic.GetProperty("severity").GetString() |> shouldEqual "error"
    diagnostic.GetProperty("code").GetString() |> shouldEqual "FS0583"
    diagnostic.GetProperty("message").GetString() |> shouldEqual "Unmatched '('"

    let range: JsonElement = diagnostic.GetProperty("range")
    range.GetProperty("startLine").GetInt32() |> shouldEqual 3
    range.GetProperty("startColumn").GetInt32() |> shouldEqual 9
    range.GetProperty("endLine").GetInt32() |> shouldEqual 3
    range.GetProperty("endColumn").GetInt32() |> shouldEqual 10

[<Test>]
let ``a failure that is not a parse failure is carried as its message alone`` () =
    let document: JsonElement =
        completed [ FormatResult.Error("a.fs", FormatException "something gave way") ]

    let file: JsonElement = files document |> List.exactlyOne

    file.GetProperty("message").GetString() |> shouldEqual "something gave way"

    file.GetProperty("diagnostics").EnumerateArray() |> shouldBeEmpty

[<Test>]
let ``output Fantomas invalidated is reported as a failure of that file`` () =
    let document: JsonElement = completed [ FormatResult.InvalidCode("a.fs", "") ]
    let file: JsonElement = files document |> List.exactlyOne
    snd (statusOf file) |> shouldEqual "error"

    file.GetProperty("message").GetString() |> shouldContainText "a.fs"

// Only a file that failed carries them, so a folder of files that were fine does not repeat a null
// message and an empty list for every one of them.
[<Test>]
let ``a file that did not fail carries neither a message nor diagnostics`` () =
    let file: JsonElement =
        completed [ FormatResult.Formatted("a.fs", "") ] |> files |> List.exactlyOne

    file.TryGetProperty "message" |> fst |> shouldEqual false
    file.TryGetProperty "diagnostics" |> fst |> shouldEqual false

// A check counts a file it could not read as changed as well as errored. One file is one entry, and
// the failure is the more useful of the two answers.
[<Test>]
let ``a file that a check could not read is reported once, as an error`` () =
    let document: JsonElement =
        checked' [] [ "a.fs", unmatchedBracket ] [ "a.fs"; "b.fs" ] []

    files document
    |> List.map statusOf
    |> shouldEqual [ "a.fs", "error"; "b.fs", "needs-formatting" ]

[<Test>]
let ``a check reports the files it ignored`` () =
    checked' [ "a.fs" ] [] [] []
    |> files
    |> List.map statusOf
    |> shouldEqual [ "a.fs", "ignored" ]

// A check used to name only the files it had a complaint about, so a caller had to read "already
// formatted" out of a file being absent. Both commands now list every file they looked at.
[<Test>]
let ``a check names the files it found nothing to say about`` () =
    checked' [ "d.fs" ] [] [ "b.fs" ] [ "a.fs"; "c.fs" ]
    |> files
    |> List.map statusOf
    |> shouldEqual
        [
            "a.fs", "unchanged"
            "b.fs", "needs-formatting"
            "c.fs", "unchanged"
            "d.fs", "ignored"
        ]

[<Test>]
let ``files are ordered by path, whichever order the run produced them in`` () =
    completed
        [
            FormatResult.Unchanged "c.fs"
            FormatResult.Unchanged "a.fs"
            FormatResult.Unchanged "b.fs"
        ]
    |> files
    |> List.map (statusOf >> fst)
    |> shouldEqual [ "a.fs"; "b.fs"; "c.fs" ]

// A path is not guaranteed to be ASCII and the document is read by a machine, so what comes back
// out of it has to be the path that went in.
[<Test>]
let ``a path that is not ASCII survives the round trip`` () =
    let document: JsonElement =
        completed [ FormatResult.Formatted("src/Café/Ünicode.fs", "") ]

    files document
    |> List.map (statusOf >> fst)
    |> shouldEqual [ "src/Café/Ünicode.fs" ]
