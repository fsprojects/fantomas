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
open Fantomas.ProfileCommand
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
    // `c.fs` was ignored, so it is counted rather than listed.
    |> shouldEqual [ "a.fs", "formatted"; "b.fs", "unchanged"; "d.fs", "error" ]

[<Test>]
let ``the document says which command produced it`` () =
    let format: JsonElement = completed []
    format.GetProperty("command").GetString() |> shouldEqual "format"

    let check: JsonElement = checked' [] [] [] []
    check.GetProperty("command").GetString() |> shouldEqual "check"

// A version number says a shape is a contract somebody is maintaining, and this one is not. It is
// here so a machine can see what a run did, which is a job that tolerates the shape moving, and
// carrying a version would mean holding a key nobody uses until the next major because one script
// somewhere parsed it.
[<Test>]
let ``the document carries no version, and promises nothing about its shape`` () =
    (completed []).TryGetProperty "version" |> fst |> shouldEqual false

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
    let rejection: Fantomas.FCS.Parse.FSharpParserDiagnostic list =
        [
            {
                Severity = Fantomas.FCS.Diagnostics.FSharpDiagnosticSeverity.Error
                SubCategory = "parse"
                Range =
                    Some(
                        Fantomas.FCS.Text.Range.mkRange
                            "tmp.fsx"
                            (Fantomas.FCS.Text.Position.mkPos 3 9)
                            (Fantomas.FCS.Text.Position.mkPos 3 10)
                    )
                ErrorNumber = Some 583
                Message = "Unmatched '('"
            }
        ]

    let document: JsonElement =
        completed [ FormatResult.InvalidCode("a.fs", "module A\n\nlet a = (1\n", rejection) ]

    let file: JsonElement = files document |> List.exactlyOne
    snd (statusOf file) |> shouldEqual "error"

    // The path is a key of its own beside the message, so the message does not repeat it.
    file.GetProperty("path").GetString() |> shouldEqual "a.fs"

    // The same wording the console prints, minus the colour and the file in front of it, rather
    // than a shorter sentence written for this document alone.
    let message: string = file.GetProperty("message").GetString()
    message |> shouldContainText "your file is unchanged"
    message |> shouldContainText "a bug in Fantomas"

    // What was wrong with the output, carried the way a parse failure's diagnostics are, so a
    // caller has the position without having to read it back out of the prose.
    let diagnostic: JsonElement =
        file.GetProperty("diagnostics").EnumerateArray()
        |> List.ofSeq
        |> List.exactlyOne

    diagnostic.GetProperty("severity").GetString() |> shouldEqual "error"
    diagnostic.GetProperty("code").GetString() |> shouldEqual "FS0583"

    diagnostic.GetProperty("range").GetProperty("startLine").GetInt32()
    |> shouldEqual 3

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

// A file an ignore file kept the run away from is neither listed nor counted. It used to be
// counted, and the number could not be honest: a pattern naming a file can be counted, and a
// pattern naming a folder cannot, because the folder is never opened. A count right about the first
// and blind to the second reads as though it covered both.
[<Test>]
let ``an ignored file is neither listed nor counted`` () =
    let document: JsonElement = checked' [ "a.fs" ] [] [] [ "b.fs" ]

    document |> files |> List.map statusOf |> shouldEqual [ "b.fs", "unchanged" ]
    document.TryGetProperty "ignored" |> fst |> shouldEqual false

[<Test>]
let ``no command carries a count of what it was kept away from`` () =
    for document in
        [
            completed [ FormatResult.Formatted("a.fs", "") ]
            checked' [] [] [] [ "a.fs" ]
        ] do
        document.TryGetProperty "ignored" |> fst |> shouldEqual false

// A check used to name only the files it had a complaint about, so a caller had to read "already
// formatted" out of a file being absent. Both commands now list every file they looked at.
[<Test>]
let ``a check names the files it found nothing to say about`` () =
    checked' [ "d.fs" ] [] [ "b.fs" ] [ "a.fs"; "c.fs" ]
    |> files
    |> List.map statusOf
    |> shouldEqual [ "a.fs", "unchanged"; "b.fs", "needs-formatting"; "c.fs", "unchanged" ]

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

[<Test>]
let ``a profile document times every file it measured`` () =
    let report: RunReport =
        profileReport
            "/tmp"
            (ProfileCommandResult.Completed
                {
                    Timings =
                        [
                            {
                                File = "b.fs"
                                LineCount = 10
                                DefineCombinations = 2
                                TimeTaken = TimeSpan.FromMilliseconds 40.0
                            }
                            {
                                File = "a.fs"
                                LineCount = 5
                                DefineCombinations = 1
                                TimeTaken = TimeSpan.FromMilliseconds 10.0
                            }
                        ]
                    Ignored = []
                    Errors = []
                    Elapsed = TimeSpan.FromMilliseconds 90.0
                })

    // Ordered by path here, where the text report orders by time. A reader wanting them by time can
    // sort them; a reader looking one file up should not have to.
    report.Files
    |> List.map (fun file -> file.Path)
    |> shouldEqual [ "a.fs"; "b.fs" ]

    // The run is not the sum of the files: reading each one and walking the folder are in it.
    report.ElapsedMilliseconds |> shouldEqual (Some 90)

    report.Files
    |> List.map (fun file -> file.Outcome)
    |> shouldEqual [ FileOutcome.Timed(5, 1, 10); FileOutcome.Timed(10, 2, 40) ]

[<Test>]
let ``only the command that measures carries an elapsed time`` () =
    // The other two would carry a null on every run to say nothing, which is what the per file keys
    // already avoid.
    let document: string =
        render (formatReport "/tmp" (FormatCommandResult.Completed [||]))

    document |> shouldNotContainText "elapsedMilliseconds"
