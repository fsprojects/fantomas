module Fantomas.Tests.EditorConfigReportTests

open NUnit.Framework
open Fantomas
open Fantomas.EditorConfig
open Fantomas.EditorConfigReport
open Fantomas.Tests.TestHelpers

let private (==) (actual: 'T) (expected: 'T) =
    Assert.That(actual, Is.EqualTo<'T> expected)

let private unknown = EditorConfigProblem.UnknownSetting
let private unrecognized = EditorConfigProblem.UnrecognizedValue

/// What a reporter over a collecting logger wrote, one string per message.
let private reportedBy (report: EditorConfigReporter -> unit) : string list =
    let logger, collected = collectingLogger ()
    report (createReporter logger)
    (collected ()).Warning

[<Test>]
let ``nothing to say about an editorconfig Fantomas can act on`` () =
    describe "/repo/.editorconfig" [] == None
    reportedBy (fun report -> report "/repo/.editorconfig" []) == []

[<Test>]
let ``every problem is named, and the origin with them`` () =
    let report =
        describe "/repo/.editorconfig" [ unknown "fsharp_wibble"; unrecognized ("fsharp_experimental_elmish", "yes") ]

    match report with
    | None -> Assert.Fail "Expected a report"
    | Some report ->
        Assert.That(report, Does.Contain "/repo/.editorconfig")
        Assert.That(report, Does.Contain "'fsharp_wibble' is not a Fantomas setting")

        Assert.That(
            report,
            Does.Contain "'fsharp_experimental_elmish' does not accept the value 'yes', so the default is used instead"
        )

[<Test>]
let ``a misspelling is answered with the spelling that works`` () =
    suggestionFor "fsharp_multiline_brackets_style"
    == Some "fsharp_multiline_bracket_style"

    suggestionFor "fsharp_max_recrd_width" == Some "fsharp_max_record_width"

[<Test>]
let ``prefixing a setting editorconfig itself defines is answered with the unprefixed name`` () =
    suggestionFor "fsharp_max_line_length" == Some "max_line_length"
    suggestionFor "fsharp_indent_size" == Some "indent_size"

[<Test>]
let ``a name nothing like a setting is not guessed at`` () = suggestionFor "fsharp_wibble" == None

[<Test>]
let ``the settings list is not written out for a value that could not be read`` () =
    // The name was right. Reading out every name Fantomas knows answers nothing.
    match describe "/repo/.editorconfig" [ unrecognized ("fsharp_max_record_width", "banana") ] with
    | None -> Assert.Fail "Expected a report"
    | Some report ->
        Assert.That(report, Does.Not.Contain "--verbosity d")
        Assert.That(report, Does.Not.Contain "fsharp_multiline_bracket_style")

[<Test>]
let ``a setting Fantomas does not have points at where the whole list is`` () =
    match describe "/repo/.editorconfig" [ unknown "fsharp_wibble" ] with
    | None -> Assert.Fail "Expected a report"
    | Some report -> Assert.That(report, Does.Contain "--verbosity d")

[<Test>]
let ``the list of supported settings names the running version and every setting it has`` () =
    let described = describeSupportedSettings ()

    Assert.That(described, Does.Contain fantomasVersion)

    for setting in supportedSettings do
        Assert.That(described, Does.Contain setting)

[<Test>]
let ``one report is written once, however many files it is found for`` () =
    let written =
        reportedBy (fun report ->
            for _ in 1..5 do
                report "/repo/.editorconfig" [ unknown "fsharp_wibble" ])

    written |> List.length == 1

[<Test>]
let ``two editorconfigs with the same problem are both reported`` () =
    let written =
        reportedBy (fun report ->
            report "/repo/one/.editorconfig" [ unknown "fsharp_wibble" ]
            report "/repo/two/.editorconfig" [ unknown "fsharp_wibble" ])

    written |> List.length == 2

[<Test>]
let ``each reporter starts with a clean record of what it has written`` () =
    // The record used to be one dictionary for the lifetime of the process, which meant a test
    // could only observe "written once" by starting a new one.
    reportedBy (fun report -> report "/repo/.editorconfig" [ unknown "fsharp_wibble" ])
    |> List.length
    == 1

    reportedBy (fun report -> report "/repo/.editorconfig" [ unknown "fsharp_wibble" ])
    |> List.length
    == 1

// Files are formatted in parallel. Written as several messages, the settings list could arrive
// between another thread's problems and the line they belong to.
[<Test>]
let ``a report is one message, problems and all`` () =
    let written =
        reportedBy (fun report ->
            report
                "/repo/.editorconfig"
                [ unknown "fsharp_wibble"; unrecognized ("fsharp_max_record_width", "banana") ])

    match written with
    | [ single ] ->
        Assert.That(single, Does.Contain "fsharp_wibble")
        Assert.That(single, Does.Contain "banana")
    | otherwise -> Assert.Fail $"Expected one message, got %A{otherwise}"

[<Test>]
let ``the supported settings are written at debug verbosity, once per run`` () =
    let logger, collected = collectingLogger ()
    let report = createReporter logger

    report "/repo/one/.editorconfig" [ unknown "fsharp_wibble" ]
    report "/repo/two/.editorconfig" [ unknown "fsharp_wobble" ]

    let debug = (collected ()).Debug
    debug |> List.length == 1
    Assert.That(List.head debug, Does.Contain "fsharp_multiline_bracket_style")

// A value is whatever someone typed after the `=`. Unquoted, an empty one vanished and one with
// spaces in it ran into the sentence around it: "does not accept the value not a bool, so there,
// so the default is used instead."
[<TestCase("", "''")>]
[<TestCase("strous trup", "'strous trup'")>]
[<TestCase("not a bool, so there", "'not a bool, so there'")>]
let ``an awkward value is delimited so the sentence still reads`` (value: string, expected: string) =
    match describe "/repo/.editorconfig" [ unrecognized ("fsharp_experimental_elmish", value) ] with
    | None -> Assert.Fail "Expected a report"
    | Some report -> Assert.That(report, Does.Contain $"does not accept the value %s{expected}, so the default")

[<Test>]
let ``a suggestion is delimited too, so the question mark is not read as part of the name`` () =
    match describe "/repo/.editorconfig" [ unknown "fsharp_max_line_length" ] with
    | None -> Assert.Fail "Expected a report"
    | Some report -> Assert.That(report, Does.Contain "Did you mean 'max_line_length'?")

[<Test>]
let ``a value carrying braces is not read as a message template`` () =
    // Serilog reads `{Foo}` in a message template as a property to fill in. The report is text
    // someone wrote in their `.editorconfig`, so it travels as a property rather than as the
    // template.
    let written =
        reportedBy (fun report -> report "/repo/.editorconfig" [ unrecognized ("fsharp_max_record_width", "{Origin}") ])

    match written with
    | [ single ] -> Assert.That(single, Does.Contain "{Origin}")
    | otherwise -> Assert.Fail $"Expected one message, got %A{otherwise}"
