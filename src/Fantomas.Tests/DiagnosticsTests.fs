module Fantomas.Tests.DiagnosticsTests

open NUnit.Framework
open FsUnit
open Fantomas
open Fantomas.FCS.Diagnostics
open Fantomas.FCS.Parse
open Fantomas.FCS.Text
open Fantomas.Theme

// Every one of these renders with no colour, so that what is asserted on is the words and the
// layout. What the colour is and where it lands is `ThemeTests` and `ReportTests`.
let private plain: Theme = Theme.plain

// Diagnostics are constructed here rather than parsed from source on purpose: the messages and
// numbers the parser produces move when the vendored compiler is bumped, and what these tests
// are about is the rendering.
let private diagnostic severity errorNumber message (startLine, startColumn) (endLine, endColumn) =
    {
        Severity = severity
        SubCategory = "parse"
        Range = Some(Range.mkRange "tmp.fsx" (Position.mkPos startLine startColumn) (Position.mkPos endLine endColumn))
        ErrorNumber = Some errorNumber
        Message = message
    }

let private error errorNumber message start finish =
    diagnostic FSharpDiagnosticSeverity.Error errorNumber message start finish

let private warning errorNumber message start finish =
    diagnostic FSharpDiagnosticSeverity.Warning errorNumber message start finish

let private source = "module A\n\nlet a = (1 + 2\n\nlet b = 2\nlet c = 3\n"

let private lines (text: string) =
    text.Replace("\r\n", "\n").Split('\n') |> Array.toList

[<Test>]
let ``a diagnostic is reported as an MSBuild style line with a one based column`` () =
    let rendered =
        Diagnostics.renderParseFailure plain "/tmp/bad.fs" "" [ error 583 "Unmatched '('" (3, 8) (3, 9) ]

    lines rendered
    |> should
        equal
        [
            "/tmp/bad.fs could not be parsed by Fantomas:"
            ""
            "/tmp/bad.fs(3,9): error FS0583: Unmatched '('"
            ""
        ]

[<Test>]
let ``the file the caller names is reported, not the one the parser was handed`` () =
    let rendered =
        Diagnostics.renderParseFailure plain "/tmp/bad.fs" "" [ error 583 "Unmatched '('" (3, 8) (3, 9) ]

    rendered |> should not' (contain "tmp.fsx")

[<Test>]
let ``diagnostics are ordered by position, not by the order the parser produced them`` () =
    let rendered =
        Diagnostics.renderParseFailure
            plain
            "bad.fs"
            ""
            [
                error 58 "Offside" (4, 0) (4, 3)
                error 3118 "Incomplete value or function definition" (3, 0) (3, 3)
            ]

    lines rendered
    |> should
        equal
        [
            "bad.fs could not be parsed by Fantomas:"
            ""
            "bad.fs(3,1): error FS3118: Incomplete value or function definition"
            "bad.fs(4,1): error FS0058: Offside"
            ""
        ]

[<Test>]
let ``a warning keeps its severity`` () =
    let rendered =
        Diagnostics.renderParseFailure
            plain
            "bad.fs"
            ""
            [ warning 1104 "Identifiers containing '@' are reserved" (2, 4) (2, 11) ]

    rendered
    |> should contain "bad.fs(2,5): warning FS1104: Identifiers containing '@' are reserved"

[<Test>]
let ``a message that spans lines is collapsed, so one diagnostic stays one line`` () =
    let rendered =
        Diagnostics.renderParseFailure plain "bad.fs" "" [ error 10 "First part.\nSecond part." (1, 0) (1, 1) ]

    rendered |> should contain "bad.fs(1,1): error FS0010: First part. Second part."

[<Test>]
let ``a diagnostic without a range is still reported`` () =
    let rendered =
        Diagnostics.renderParseFailure
            plain
            "bad.fs"
            ""
            [
                {
                    Severity = FSharpDiagnosticSeverity.Error
                    SubCategory = "parse"
                    Range = None
                    ErrorNumber = None
                    Message = "Something went wrong"
                }
            ]

    rendered |> should contain "bad.fs: error FS0000: Something went wrong"

[<Test>]
let ``the snippet shows two lines either side with a caret under the range`` () =
    let rendered =
        Diagnostics.renderParseFailure plain "bad.fs" source [ error 583 "Unmatched '('" (3, 8) (3, 9) ]

    lines rendered
    |> should
        equal
        [
            "bad.fs could not be parsed by Fantomas:"
            ""
            "bad.fs(3,9): error FS0583: Unmatched '('"
            ""
            "1 | module A"
            "2 | "
            "3 | let a = (1 + 2"
            "  |         ^"
            "4 | "
            "5 | let b = 2"
            ""
        ]

[<Test>]
let ``the caret goes on the first error, not on a warning that sorts ahead of it`` () =
    let rendered =
        Diagnostics.renderParseFailure
            plain
            "bad.fs"
            source
            [
                warning 1104 "Reserved" (1, 0) (1, 6)
                error 583 "Unmatched '('" (3, 8) (3, 9)
            ]

    rendered |> should contain "3 | let a = (1 + 2"
    rendered |> should contain "  |         ^"

[<Test>]
let ``the window is clipped at the start and the end of the file`` () =
    let rendered =
        Diagnostics.renderParseFailure plain "bad.fs" "let a = 1\n" [ error 10 "Unexpected" (1, 0) (1, 3) ]

    lines rendered
    |> should
        equal
        [
            "bad.fs could not be parsed by Fantomas:"
            ""
            "bad.fs(1,1): error FS0010: Unexpected"
            ""
            "1 | let a = 1"
            "  | ^^^"
            "2 | "
            ""
        ]

[<Test>]
let ``tabs are expanded in the line and under the caret, so the two stay aligned`` () =
    let rendered =
        Diagnostics.renderParseFailure
            plain
            "bad.fs"
            "module A\n\n\tlet a = (1\n"
            [ error 583 "Unmatched '('" (3, 9) (3, 10) ]

    lines rendered
    |> should
        equal
        [
            "bad.fs could not be parsed by Fantomas:"
            ""
            "bad.fs(3,10): error FS0583: Unmatched '('"
            ""
            "1 | module A"
            "2 | "
            "3 |     let a = (1"
            "  |             ^"
            "4 | "
            ""
        ]

[<Test>]
let ``a tab inside the range widens the caret run by as much as it widened the line`` () =
    let rendered =
        Diagnostics.renderParseFailure plain "bad.fs" "module A\n\nlet a\t= 1\n" [ error 10 "Unexpected" (3, 3) (3, 7) ]

    lines rendered
    |> should
        equal
        [
            "bad.fs could not be parsed by Fantomas:"
            ""
            "bad.fs(3,4): error FS0010: Unexpected"
            ""
            "1 | module A"
            "2 | "
            "3 | let a    = 1"
            "  |    ^^^^^^^"
            "4 | "
            ""
        ]

[<Test>]
let ``a range that runs past its first line is underlined to the end of that line`` () =
    let rendered =
        Diagnostics.renderParseFailure plain "bad.fs" source [ error 3118 "Incomplete" (3, 0) (5, 9) ]

    rendered |> should contain "  | ^^^^^^^^^^^^^^"

[<Test>]
let ``a range beyond the end of the file leaves the snippet out rather than throwing`` () =
    let rendered =
        Diagnostics.renderParseFailure plain "bad.fs" "let a = 1\n" [ error 10 "Unexpected" (40, 0) (40, 3) ]

    lines rendered
    |> should
        equal
        [
            "bad.fs could not be parsed by Fantomas:"
            ""
            "bad.fs(40,1): error FS0010: Unexpected"
            ""
        ]

[<Test>]
let ``without source there is no snippet`` () =
    let rendered =
        Diagnostics.renderParseFailure plain "bad.fs" "" [ error 583 "Unmatched '('" (3, 8) (3, 9) ]

    lines rendered |> List.length |> should equal 4

[<Test>]
let ``an exception that is not a parse failure is not this module's to describe`` () =
    Diagnostics.describeParseFailure plain "bad.fs" (fun () -> source) (exn "boom")
    |> should equal None

// An invariant violation is rendered from a construct Fantomas could not model rather than from a
// parser diagnostic, so it is built here the same way: what these tests are about is the rendering.
let private externSource: string =
    "module A\n\nlet before = 1\n\nextern int64 private f(byte[] | null value)\n\nlet after = 2\n"

let private violation () : Fantomas.Core.InvariantViolationException =
    Fantomas.Core.InvariantViolationException(
        "no Oak node is defined for this type: SynType.App",
        Range.mkRange "tmp.fsx" (Position.mkPos 5 23) (Position.mkPos 5 27),
        "App\n  (LongIdent (SynLongIdent ([byte], [], [None])), None, [], [], None, false)"
    )

[<Test>]
let ``an invariant violation is positioned and shown with a caret under the construct`` () =
    let rendered =
        Diagnostics.renderInvariantViolation plain "bad.fs" externSource false (violation ())

    lines rendered
    |> should
        equal
        [
            "bad.fs could not be formatted by Fantomas:"
            ""
            "bad.fs(5,24): error: no Oak node is defined for this type: SynType.App"
            ""
            "3 | let before = 1"
            "4 | "
            "5 | extern int64 private f(byte[] | null value)"
            "  |                        ^^^^"
            "6 | "
            "7 | let after = 2"
            ""
            "This is a bug in Fantomas, not a problem with your code. Please report it with the snippet above via https://fsprojects.github.io/fantomas-tools/, or at https://github.com/fsprojects/fantomas/issues/new if the file is too large for the tool to carry."
            ""
        ]

[<Test>]
let ``the path comes from the caller, not from the name the parser was handed`` () =
    let rendered =
        Diagnostics.renderInvariantViolation plain "src/XAttr.fs" externSource false (violation ())

    rendered |> should haveSubstring "src/XAttr.fs(5,24)"
    rendered |> should not' (haveSubstring "tmp.fsx")

[<Test>]
let ``the syntax tree node is left out by default and shown when asked for`` () =
    Diagnostics.renderInvariantViolation plain "bad.fs" externSource false (violation ())
    |> should not' (haveSubstring "SynLongIdent")

    let verbose =
        Diagnostics.renderInvariantViolation plain "bad.fs" externSource true (violation ())

    verbose |> should haveSubstring "Syntax tree node:"
    verbose |> should haveSubstring "SynLongIdent"

[<Test>]
let ``an invariant violation without source is still positioned`` () =
    let rendered =
        Diagnostics.renderInvariantViolation plain "bad.fs" "" false (violation ())

    rendered |> should haveSubstring "bad.fs(5,24)"
    rendered |> should not' (haveSubstring "^^^^")

[<Test>]
let ``an exception that is not an invariant violation is not this module's to describe`` () =
    Diagnostics.describeInvariantViolation plain "bad.fs" (fun () -> externSource) false (exn "boom")
    |> should equal None

// What colour lands where. The report is one block of text rather than a row of fields, so what is
// pinned is that each part carries its own colour and that removing the colour leaves the plain
// report exactly as it is.
let private coloured: Theme =
    {
        Palette = Palette.EightBit
        Glyphs = GlyphSet.Unicode
    }

let private anyEscapeSequence: System.Text.RegularExpressions.Regex =
    System.Text.RegularExpressions.Regex(@"\u001b\[[0-9;]*m")

[<Test>]
let ``a parse failure colours the place, the severity, the number, the gutter and the carets`` () =
    let rendered: string =
        Diagnostics.renderParseFailure
            coloured
            "bad.fs"
            source
            [
                error 583 "Unmatched '('" (3, 8) (3, 9)
                warning 64 "This construct" (5, 0) (5, 3)
            ]

    // Teal for somewhere the reader can go: the file, and each diagnostic's position in it.
    rendered
    |> should haveSubstring "\u001b[38;5;38mbad.fs\u001b[0m could not be parsed"

    rendered |> should haveSubstring "\u001b[38;5;38mbad.fs(3,9)\u001b[0m:"
    // The severity is the outcome it is: red for an error, yellow for a warning.
    rendered |> should haveSubstring "\u001b[31merror\u001b[0m"
    rendered |> should haveSubstring "\u001b[33mwarning\u001b[0m"
    // Grey for the number, which is scaffolding a reader looks up rather than reads.
    rendered |> should haveSubstring "\u001b[38;5;245mFS0583\u001b[0m"
    // Dim for the gutter, red for the carets.
    rendered |> should haveSubstring "\u001b[2m3 |\u001b[0m"
    rendered |> should haveSubstring "\u001b[31m^\u001b[0m"

[<Test>]
let ``an invariant violation colours the place, the severity and the two links`` () =
    let rendered: string =
        Diagnostics.renderInvariantViolation coloured "bad.fs" externSource false (violation ())

    rendered
    |> should haveSubstring "\u001b[38;5;38mbad.fs\u001b[0m could not be formatted"

    rendered |> should haveSubstring "\u001b[31merror\u001b[0m:"

    rendered
    |> should haveSubstring "\u001b[38;5;38mhttps://fsprojects.github.io/fantomas-tools/\u001b[0m"

[<Test>]
let ``colour changes what is written but not what it says`` () =
    // Which is what makes the plain rendering the same report rather than a lesser one: a redirected
    // stream and the daemon lose the colour and nothing else.
    let of' (theme: Theme) : string =
        Diagnostics.renderParseFailure theme "bad.fs" source [ error 583 "Unmatched '('" (3, 8) (3, 9) ]

    anyEscapeSequence.Replace(of' coloured, "") |> should equal (of' plain)
