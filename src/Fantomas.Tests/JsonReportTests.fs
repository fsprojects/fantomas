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
        completed
            [
                FormatResult.InvalidCode(
                    "a.fs",
                    "module A\n\nlet a = (1\n",
                    [ ValidationIssue.NotValidFSharp([], rejection) ]
                )
            ]

    let file: JsonElement = files document |> List.exactlyOne
    snd (statusOf file) |> shouldEqual "error"

    // The path is a key of its own beside the message, so the message does not repeat it.
    file.GetProperty("path").GetString() |> shouldEqual "a.fs"

    // The same wording the console prints, minus the colour and the file in front of it, rather
    // than a shorter sentence written for this document alone.
    let message: string = file.GetProperty("message").GetString()
    message |> shouldContainText "Your file is unchanged"
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

    // The combinations it was reported under, the one without defines here.
    diagnostic.GetProperty("reportedUnder").EnumerateArray()
    |> Seq.map (fun (defines: JsonElement) -> defines.GetArrayLength())
    |> List.ofSeq
    |> shouldEqual [ 0 ]

    file.GetProperty("missingComments").GetArrayLength() |> shouldEqual 0

// Output that is valid F# and still refused, because it lost a comment. The comment is listed as
// the file has it, with where it is in the file.
[<Test>]
let ``the comments a refused output is missing are in the document`` () =
    let document: JsonElement =
        completed
            [
                FormatResult.InvalidCode(
                    "a.fs",
                    "let a = 1\n",
                    [
                        ValidationIssue.MissingComment
                            {
                                Range = Range.mkRange "" (Position.mkPos 1 10) (Position.mkPos 1 16)
                                Text = "// one"
                            }
                    ]
                )
            ]

    let file: JsonElement = files document |> List.exactlyOne
    snd (statusOf file) |> shouldEqual "error"
    file.GetProperty("diagnostics").GetArrayLength() |> shouldEqual 0

    let comment: JsonElement =
        file.GetProperty("missingComments").EnumerateArray() |> Seq.exactlyOne

    comment.GetProperty("text").GetString() |> shouldEqual "// one"

    comment.GetProperty("range").GetProperty("startLine").GetInt32()
    |> shouldEqual 1

    comment.GetProperty("range").GetProperty("startColumn").GetInt32()
    |> shouldEqual 11

// Written although a format run refuses it: the document says so, and what was wrong, which the
// warning only told standard error.
[<Test>]
let ``a file written by force says so, and what was wrong with it`` () =
    let document: JsonElement =
        completed
            [
                FormatResult.Forced(
                    "a.fs",
                    "let a = 1\n",
                    [
                        ValidationIssue.MissingComment
                            {
                                Range = Range.mkRange "" (Position.mkPos 1 10) (Position.mkPos 1 16)
                                Text = "// one"
                            }
                    ]
                )
            ]

    let file: JsonElement = files document |> List.exactlyOne
    snd (statusOf file) |> shouldEqual "formatted"
    file.GetProperty("forced").GetBoolean() |> shouldEqual true
    file.GetProperty("diagnostics").GetArrayLength() |> shouldEqual 0

    file.GetProperty("missingComments").[0].GetProperty("text").GetString()
    |> shouldEqual "// one"

// Only a file that failed carries them, so a folder of files that were fine does not repeat a null
// message and an empty list for every one of them.
[<Test>]
let ``a file that did not fail carries neither a message nor diagnostics`` () =
    let file: JsonElement =
        completed [ FormatResult.Formatted("a.fs", "") ] |> files |> List.exactlyOne

    file.TryGetProperty "message" |> fst |> shouldEqual false
    file.TryGetProperty "diagnostics" |> fst |> shouldEqual false
    file.TryGetProperty "missingComments" |> fst |> shouldEqual false

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

// ---- the doctor document ----

let private diagnosed (report: Fantomas.DoctorCommand.DoctorReport) : JsonElement =
    JsonDocument
        .Parse(renderDoctorReport workingDirectory (Fantomas.DoctorCommand.DoctorCommandResult.Completed report))
        .RootElement

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
        Ignore = Some(Fantomas.DoctorCommand.IgnoreStep.Governed("/repo/.fantomasignore", false, [], []))
        Settings = Some(Fantomas.EditorConfigFiles.withoutEditorConfig FormatConfig.Default)
        Parse = Some(Fantomas.DoctorCommand.ParseStep.Parsed [ [] ])
        Format =
            Some(
                Fantomas.DoctorCommand.FormatStep.Produced(
                    "let a = 1\n",
                    Fantomas.DoctorCommand.FormatChange.Reformatted(4, 25)
                )
            )
        Validity = Some Fantomas.DoctorCommand.ValidityStep.Valid
        Trivia = Some Fantomas.DoctorCommand.TriviaStep.Kept
        Idempotency = Some Fantomas.DoctorCommand.IdempotencyStep.Idempotent
    }

let private statusAt (document: JsonElement) (key: string) : string =
    document.GetProperty(key).GetProperty("status").GetString()

[<Test>]
let ``a doctor document carries a key per step`` () =
    let document: JsonElement = diagnosed healthy

    document.GetProperty("command").GetString() |> shouldEqual "doctor"
    statusAt document "file" |> shouldEqual "candidate"

    document.GetProperty("file").GetProperty("kind").GetString()
    |> shouldEqual "implementation"

    statusAt document "ignore" |> shouldEqual "not-ignored"
    statusAt document "format" |> shouldEqual "changed"
    statusAt document "validity" |> shouldEqual "valid"
    statusAt document "trivia" |> shouldEqual "kept"
    statusAt document "idempotency" |> shouldEqual "idempotent"

    document.GetProperty("format").GetProperty("firstChangedLine").GetInt32()
    |> shouldEqual 4

    document.GetProperty("format").GetProperty("lineCountAfter").GetInt32()
    |> shouldEqual 25

[<Test>]
let ``a step the walk never reached is null rather than absent`` () =
    // A key that is sometimes missing is a trap in the languages most likely to be reading this,
    // and a step nothing was asked about is not a step that found nothing.
    let ignored: Fantomas.DoctorCommand.DoctorReport =
        { healthy with
            Ignore = Some(Fantomas.DoctorCommand.IgnoreStep.Governed("/repo/.fantomasignore", true, [], []))
            Settings = None
            Parse = None
            Format = None
            Validity = None
            Trivia = None
            Idempotency = None
        }

    let document: JsonElement = diagnosed ignored

    for key in [ "configuration"; "format"; "validity"; "trivia"; "idempotency" ] do
        document.GetProperty(key).ValueKind |> shouldEqual JsonValueKind.Null

    statusAt document "ignore" |> shouldEqual "ignored"

[<Test>]
let ``the ignore files that had no say are carried, whether or not they would have decided`` () =
    // Every one of them rather than only those that disagree: the reader here is a program, which
    // can filter on `wouldIgnore` itself and cannot ask for the ones that were left out.
    let document: JsonElement =
        diagnosed
            { healthy with
                Ignore =
                    Some(
                        Fantomas.DoctorCommand.IgnoreStep.Governed(
                            "/repo/src/.fantomasignore",
                            false,
                            [],
                            [
                                {
                                    Path = "/repo/.fantomasignore"
                                    WouldIgnore = true
                                    Matches =
                                        [
                                            {
                                                LineNumber = 1
                                                Pattern = "*.g.fs"
                                                Negated = false
                                            }
                                        ]
                                }
                                {
                                    Path = "/.fantomasignore"
                                    WouldIgnore = false
                                    Matches = []
                                }
                            ]
                        )
                    )
            }

    let shadowed: JsonElement list =
        document.GetProperty("ignore").GetProperty("shadowed").EnumerateArray()
        |> List.ofSeq

    shadowed
    |> List.map (fun entry -> entry.GetProperty("path").GetString(), entry.GetProperty("wouldIgnore").GetBoolean())
    |> shouldEqual [ ("/repo/.fantomasignore", true); ("/.fantomasignore", false) ]

    shadowed.[0].GetProperty("matches").EnumerateArray()
    |> Seq.map (fun m -> m.GetProperty("pattern").GetString())
    |> List.ofSeq
    |> shouldEqual [ "*.g.fs" ]

[<Test>]
let ``a file with nothing above its ignore file still carries the key`` () =
    let document: JsonElement =
        diagnosed
            { healthy with
                Ignore = Some(Fantomas.DoctorCommand.IgnoreStep.Governed("/repo/.fantomasignore", false, [], []))
            }

    document.GetProperty("ignore").GetProperty("shadowed").GetArrayLength()
    |> shouldEqual 0

    // The same for a file no ignore file governs at all, for the reason every other key here is
    // written whether or not it has anything in it.
    (diagnosed
        { healthy with
            Ignore = Some Fantomas.DoctorCommand.IgnoreStep.NoIgnoreFile
        })
        .GetProperty("ignore")
        .GetProperty("shadowed")
        .GetArrayLength()
    |> shouldEqual 0

[<Test>]
let ``every setting is carried, with what set it or nothing`` () =
    // A screen is finite and a document is not, so where the text report shows the short list this
    // carries the whole answer and `setBy` is what tells the two apart.
    let settings: JsonElement list =
        (diagnosed healthy).GetProperty("configuration").GetProperty("settings").EnumerateArray()
        |> List.ofSeq

    settings.Length
    |> shouldEqual (List.length Fantomas.EditorConfig.supportedSettings)

    settings
    |> List.forall (fun setting -> setting.GetProperty("setBy").ValueKind = JsonValueKind.Null)
    |> shouldEqual true

[<Test>]
let ``the pattern that matched is carried with its line number`` () =
    let matched: Fantomas.DoctorCommand.DoctorReport =
        { healthy with
            Ignore =
                Some(
                    Fantomas.DoctorCommand.IgnoreStep.Governed(
                        "/repo/.fantomasignore",
                        true,
                        [
                            {
                                LineNumber = 4
                                Pattern = "obj/"
                                Negated = false
                            }
                        ],
                        []
                    )
                )
            Settings = None
            Parse = None
            Format = None
            Validity = None
            Trivia = None
            Idempotency = None
        }

    let first: JsonElement =
        (diagnosed matched).GetProperty("ignore").GetProperty("matches").EnumerateArray()
        |> Seq.head

    first.GetProperty("line").GetInt32() |> shouldEqual 4
    first.GetProperty("pattern").GetString() |> shouldEqual "obj/"
    first.GetProperty("negated").GetBoolean() |> shouldEqual false

[<Test>]
let ``a path that is not one file is the document's error, with every step null`` () =
    let document: JsonElement =
        JsonDocument
            .Parse(
                renderDoctorReport
                    workingDirectory
                    (Fantomas.DoctorCommand.DoctorCommandResult.NotOneFile(Fantomas.Arguments.InputPath.Folder "src"))
            )
            .RootElement

    document.GetProperty("exitCode").GetInt32() |> shouldEqual 1
    document.GetProperty("error").GetString() |> shouldContainText "one file"

    // Every step a report has, so that a reader can take each key for granted.
    for step in
        [
            "file"
            "ignore"
            "configuration"
            "parse"
            "format"
            "validity"
            "trivia"
            "idempotency"
        ] do
        document.GetProperty(step).ValueKind |> shouldEqual JsonValueKind.Null

// ---- one case per key the doctor document can carry ----

let private fileStatusOf (step: Fantomas.DoctorCommand.FileStep) : JsonElement =
    (diagnosed { healthy with File = step }).GetProperty "file"

[<Test>]
let ``each way the file step can end has a status of its own`` () =
    let statusOf (step: Fantomas.DoctorCommand.FileStep) : string =
        (fileStatusOf step).GetProperty("status").GetString()

    statusOf (Fantomas.DoctorCommand.FileStep.NotFound "/repo/A.fs")
    |> shouldEqual "not-found"

    statusOf (Fantomas.DoctorCommand.FileStep.NotFSharp "/repo/A.md")
    |> shouldEqual "not-fsharp"

[<Test>]
let ``the kind of file is carried, and the folder a walk will not open`` () =
    let candidate (kind: Fantomas.DoctorCommand.FileKind) (under: string option) : JsonElement =
        fileStatusOf (
            Fantomas.DoctorCommand.FileStep.Candidate
                {
                    Path = "/repo/A.fs"
                    Kind = kind
                    LineCount = 20
                    UnreachableUnder = under
                }
        )

    (candidate Fantomas.DoctorCommand.FileKind.Signature None).GetProperty("kind").GetString()
    |> shouldEqual "signature"

    (candidate Fantomas.DoctorCommand.FileKind.Script None).GetProperty("kind").GetString()
    |> shouldEqual "script"

    (candidate Fantomas.DoctorCommand.FileKind.Implementation (Some "/repo/obj"))
        .GetProperty("unreachableUnder")
        .GetString()
    |> shouldEqual "/repo/obj"

[<Test>]
let ``a file with no ignore file above it says so, and names none`` () =
    let ignore: JsonElement =
        (diagnosed
            { healthy with
                Ignore = Some Fantomas.DoctorCommand.IgnoreStep.NoIgnoreFile
            })
            .GetProperty
            "ignore"

    ignore.GetProperty("status").GetString() |> shouldEqual "no-ignore-file"
    ignore.GetProperty("ignoreFile").ValueKind |> shouldEqual JsonValueKind.Null
    ignore.GetProperty("matches").GetArrayLength() |> shouldEqual 0

[<Test>]
let ``a setting an .editorconfig set names the file that set it`` () =
    let resolved: Fantomas.EditorConfigFiles.ResolvedConfig =
        let plain: Fantomas.EditorConfigFiles.ResolvedConfig =
            Fantomas.EditorConfigFiles.withoutEditorConfig FormatConfig.Default

        { plain with
            EditorConfigFiles = [ "/repo/.editorconfig" ]
            Settings =
                plain.Settings
                |> List.map (fun (setting: Fantomas.EditorConfigFiles.ResolvedSetting) ->
                    if setting.Setting <> "max_line_length" then
                        setting
                    else

                    { setting with
                        SetBy = Some "/repo/.editorconfig"
                    }
                )
        }

    let configuration: JsonElement =
        (diagnosed
            { healthy with
                Settings = Some resolved
            })
            .GetProperty
            "configuration"

    configuration.GetProperty("editorConfigFiles").GetArrayLength() |> shouldEqual 1

    configuration.GetProperty("settings").EnumerateArray()
    |> Seq.find (fun setting -> setting.GetProperty("setting").GetString() = "max_line_length")
    |> fun setting -> setting.GetProperty("setBy").GetString()
    |> shouldEqual "/repo/.editorconfig"

[<Test>]
let ``a setting Fantomas cannot use is carried with what is wrong with it`` () =
    let problems: JsonElement list =
        (diagnosed
            { healthy with
                Settings =
                    Some
                        { Fantomas.EditorConfigFiles.withoutEditorConfig FormatConfig.Default with
                            Problems =
                                [
                                    Fantomas.EditorConfig.EditorConfigProblem.UnknownSetting "fsharp_nope"
                                    Fantomas.EditorConfig.EditorConfigProblem.UnrecognizedValue(
                                        "fsharp_max_record_width",
                                        "banana"
                                    )
                                ]
                        }
            })
            .GetProperty("configuration")
            .GetProperty("problems")
            .EnumerateArray()
        |> List.ofSeq

    problems
    |> List.map (fun p -> p.GetProperty("status").GetString())
    |> shouldEqual [ "unknown-setting"; "unrecognized-value" ]

    problems.[1].GetProperty("value").GetString() |> shouldEqual "banana"

    problems.[0].GetProperty("message").GetString()
    |> shouldContainText "is not a Fantomas setting"

[<Test>]
let ``a file that needs no formatting is unchanged rather than changed`` () =
    let format: JsonElement =
        (diagnosed
            { healthy with
                Format =
                    Some(Fantomas.DoctorCommand.FormatStep.Produced("", Fantomas.DoctorCommand.FormatChange.Nothing))
            })
            .GetProperty
            "format"

    format.GetProperty("status").GetString() |> shouldEqual "unchanged"

[<Test>]
let ``a file whose only fault is its line endings has a status of its own`` () =
    // Nothing about the lines changes, so `status` is the only thing telling this apart from a
    // file that needs nothing at all.
    let format: JsonElement =
        (diagnosed
            { healthy with
                Format =
                    Some(
                        Fantomas.DoctorCommand.FormatStep.Produced(
                            "",
                            Fantomas.DoctorCommand.FormatChange.LineEndingsOnly
                        )
                    )
            })
            .GetProperty
            "format"

    format.GetProperty("status").GetString() |> shouldEqual "line-endings"

[<Test>]
let ``formatting that failed carries what went wrong and where`` () =
    let format: JsonElement =
        (diagnosed
            { healthy with
                Format = Some(Fantomas.DoctorCommand.FormatStep.Failed unmatchedBracket)
                Validity = None
                Trivia = None
                Idempotency = None
            })
            .GetProperty
            "format"

    format.GetProperty("status").GetString() |> shouldEqual "failed"

    format.GetProperty("message").GetString()
    |> shouldContainText "could not be parsed by Fantomas"

    format.GetProperty("diagnostics").EnumerateArray()
    |> Seq.head
    |> fun diagnostic -> diagnostic.GetProperty("code").GetString()
    |> shouldEqual "FS0583"

[<Test>]
let ``output Fantomas will not accept carries what it would not accept about it`` () =
    let refused: FSharpParserDiagnostic list =
        match unmatchedBracket with
        | :? ParseException as failure -> failure.Diagnostics
        | other -> failwith $"Expected a parse failure to take the diagnostics from, got %A{other}"

    let validity: JsonElement =
        (diagnosed
            { healthy with
                Validity =
                    Some(Fantomas.DoctorCommand.ValidityStep.Invalid [ ValidationIssue.NotValidFSharp([], refused) ])
                Trivia = None
                Idempotency = None
            })
            .GetProperty
            "validity"

    validity.GetProperty("status").GetString() |> shouldEqual "invalid"
    validity.GetProperty("diagnostics").GetArrayLength() |> shouldEqual 1

[<Test>]
let ``the parse step carries the define combinations`` () =
    let parse: JsonElement =
        (diagnosed
            { healthy with
                Parse = Some(Fantomas.DoctorCommand.ParseStep.Parsed [ []; [ "DEBUG" ] ])
            })
            .GetProperty
            "parse"

    parse.GetProperty("status").GetString() |> shouldEqual "parsed"

    parse.GetProperty("defines").EnumerateArray()
    |> Seq.map (fun (defines: JsonElement) ->
        defines.EnumerateArray()
        |> Seq.map (fun (define: JsonElement) -> define.GetString())
        |> List.ofSeq
    )
    |> List.ofSeq
    |> shouldEqual [ []; [ "DEBUG" ] ]

[<Test>]
let ``every comment and directive the result does not keep is in the document`` () =
    let trivia: JsonElement =
        (diagnosed
            { healthy with
                Trivia =
                    Some(
                        Fantomas.DoctorCommand.TriviaStep.Changed
                            [
                                ValidationIssue.CommentsChanged(
                                    [ "FOO" ],
                                    [
                                        {
                                            Range = Range.mkRange "" (Position.mkPos 2 0) (Position.mkPos 2 6)
                                            Text = "// one"
                                        }
                                    ],
                                    []
                                )
                                ValidationIssue.DirectivesChanged([], [], [])
                            ]
                    )
            })
            .GetProperty
            "trivia"

    trivia.GetProperty("status").GetString() |> shouldEqual "changed"

    let changes: JsonElement list =
        trivia.GetProperty("changes").EnumerateArray() |> List.ofSeq

    List.length changes |> shouldEqual 2

    let comments: JsonElement = List.head changes
    comments.GetProperty("kind").GetString() |> shouldEqual "comments"
    comments.GetProperty("defines").[0].GetString() |> shouldEqual "FOO"

    comments.GetProperty("missing").[0].GetProperty("text").GetString()
    |> shouldEqual "// one"

    comments.GetProperty("missing").[0].GetProperty("range").GetProperty("startLine").GetInt32()
    |> shouldEqual 2

    comments.GetProperty("added").GetArrayLength() |> shouldEqual 0

    (List.last changes).GetProperty("kind").GetString() |> shouldEqual "directives"

[<Test>]
let ``a comment the search did not find names no define combination`` () =
    let change: JsonElement =
        (diagnosed
            { healthy with
                Trivia =
                    Some(
                        Fantomas.DoctorCommand.TriviaStep.Changed
                            [
                                ValidationIssue.MissingComment
                                    {
                                        Range = Range.mkRange "" (Position.mkPos 2 0) (Position.mkPos 2 4)
                                        Text = "// a"
                                    }
                            ]
                    )
            })
            .GetProperty("trivia")
            .GetProperty("changes")
            .[0]

    change.GetProperty("kind").GetString() |> shouldEqual "search"
    // An empty list would read as the combination without defines.
    change.TryGetProperty("defines") |> fst |> shouldEqual false

    change.GetProperty("missing").[0].GetProperty("text").GetString()
    |> shouldEqual "// a"

[<Test>]
let ``a comparison that failed is a change of its own kind`` () =
    let change: JsonElement =
        (diagnosed
            { healthy with
                Trivia =
                    Some(
                        Fantomas.DoctorCommand.TriviaStep.Changed
                            [
                                ValidationIssue.CheckFailed(Validations.TriviaComparison, exn "no node for this")
                            ]
                    )
            })
            .GetProperty("trivia")
            .GetProperty("changes")
            .[0]

    change.GetProperty("kind").GetString() |> shouldEqual "failed"
    change.GetProperty("message").GetString() |> shouldEqual "no node for this"

[<Test>]
let ``each way the idempotency step can end has a status of its own`` () =
    let idempotency (step: Fantomas.DoctorCommand.IdempotencyStep) : JsonElement =
        (diagnosed { healthy with Idempotency = Some step }).GetProperty "idempotency"

    let disagreed: JsonElement =
        idempotency (Fantomas.DoctorCommand.IdempotencyStep.NotIdempotent(2, "// a\nlet a = 1\n", "// a\nlet a =  1\n"))

    // The line each pass has where they part.
    disagreed.GetProperty("status").GetString() |> shouldEqual "not-idempotent"
    disagreed.GetProperty("line").GetInt32() |> shouldEqual 2
    disagreed.GetProperty("afterFirstPass").GetString() |> shouldEqual "let a = 1"
    disagreed.GetProperty("afterSecondPass").GetString() |> shouldEqual "let a =  1"

    let failed: JsonElement =
        idempotency (Fantomas.DoctorCommand.IdempotencyStep.Failed(exn "the second pass fell over"))

    failed.GetProperty("status").GetString() |> shouldEqual "failed"

    failed.GetProperty("message").GetString()
    |> shouldEqual "the second pass fell over"

[<Test>]
let ``a run that fell over is the document's error, with every step null`` () =
    let document: JsonElement =
        JsonDocument
            .Parse(
                renderDoctorReport
                    workingDirectory
                    (Fantomas.DoctorCommand.DoctorCommandResult.Failed(exn "the disk went away"))
            )
            .RootElement

    document.GetProperty("exitCode").GetInt32() |> shouldEqual 1
    document.GetProperty("error").GetString() |> shouldEqual "the disk went away"

    for key in
        [
            "file"
            "ignore"
            "configuration"
            "format"
            "validity"
            "trivia"
            "idempotency"
        ] do
        document.GetProperty(key).ValueKind |> shouldEqual JsonValueKind.Null

[<Test>]
let ``the document is written to the writer it is given, and answers with the exit code`` () =
    let writer: IO.StringWriter = new IO.StringWriter()

    let code: int =
        reportDoctorCommand workingDirectory writer (Fantomas.DoctorCommand.DoctorCommandResult.Completed healthy)

    code |> shouldEqual 0
    writer.ToString() |> shouldContainText "\"command\": \"doctor\""

[<Test>]
let ``a walk that stopped at the file step carries the file and nulls the rest`` () =
    let document: JsonElement =
        diagnosed
            {
                File = Fantomas.DoctorCommand.FileStep.NotFound "/repo/A.fs"
                Ignore = None
                Settings = None
                Parse = None
                Format = None
                Validity = None
                Trivia = None
                Idempotency = None
            }

    document.GetProperty("file").GetProperty("status").GetString()
    |> shouldEqual "not-found"

    for key in [ "ignore"; "configuration"; "format"; "validity"; "trivia"; "idempotency" ] do
        document.GetProperty(key).ValueKind |> shouldEqual JsonValueKind.Null
