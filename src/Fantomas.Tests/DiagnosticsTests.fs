module Fantomas.Tests.DiagnosticsTests

open NUnit.Framework
open FsUnit
open Fantomas
open Fantomas.FCS.Diagnostics
open Fantomas.FCS.Parse
open Fantomas.FCS.Text

// Diagnostics are constructed here rather than parsed from source on purpose: the messages and
// numbers the parser produces move when the vendored compiler is bumped, and what these tests
// are about is the rendering.
let private diagnostic severity errorNumber message (startLine, startColumn) (endLine, endColumn) =
    { Severity = severity
      SubCategory = "parse"
      Range = Some(Range.mkRange "tmp.fsx" (Position.mkPos startLine startColumn) (Position.mkPos endLine endColumn))
      ErrorNumber = Some errorNumber
      Message = message }

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
        Diagnostics.renderParseFailure "/tmp/bad.fs" "" [ error 583 "Unmatched '('" (3, 8) (3, 9) ]

    lines rendered
    |> should
        equal
        [ "Fantomas could not parse /tmp/bad.fs:"
          ""
          "/tmp/bad.fs(3,9): error FS0583: Unmatched '('"
          "" ]

[<Test>]
let ``the file the caller names is reported, not the one the parser was handed`` () =
    let rendered =
        Diagnostics.renderParseFailure "/tmp/bad.fs" "" [ error 583 "Unmatched '('" (3, 8) (3, 9) ]

    rendered |> should not' (contain "tmp.fsx")

[<Test>]
let ``diagnostics are ordered by position, not by the order the parser produced them`` () =
    let rendered =
        Diagnostics.renderParseFailure
            "bad.fs"
            ""
            [ error 58 "Offside" (4, 0) (4, 3)
              error 3118 "Incomplete value or function definition" (3, 0) (3, 3) ]

    lines rendered
    |> should
        equal
        [ "Fantomas could not parse bad.fs:"
          ""
          "bad.fs(3,1): error FS3118: Incomplete value or function definition"
          "bad.fs(4,1): error FS0058: Offside"
          "" ]

[<Test>]
let ``a warning keeps its severity`` () =
    let rendered =
        Diagnostics.renderParseFailure
            "bad.fs"
            ""
            [ warning 1104 "Identifiers containing '@' are reserved" (2, 4) (2, 11) ]

    rendered
    |> should contain "bad.fs(2,5): warning FS1104: Identifiers containing '@' are reserved"

[<Test>]
let ``a message that spans lines is collapsed, so one diagnostic stays one line`` () =
    let rendered =
        Diagnostics.renderParseFailure "bad.fs" "" [ error 10 "First part.\nSecond part." (1, 0) (1, 1) ]

    rendered |> should contain "bad.fs(1,1): error FS0010: First part. Second part."

[<Test>]
let ``a diagnostic without a range is still reported`` () =
    let rendered =
        Diagnostics.renderParseFailure
            "bad.fs"
            ""
            [ { Severity = FSharpDiagnosticSeverity.Error
                SubCategory = "parse"
                Range = None
                ErrorNumber = None
                Message = "Something went wrong" } ]

    rendered |> should contain "bad.fs: error FS0000: Something went wrong"

[<Test>]
let ``the snippet shows two lines either side with a caret under the range`` () =
    let rendered =
        Diagnostics.renderParseFailure "bad.fs" source [ error 583 "Unmatched '('" (3, 8) (3, 9) ]

    lines rendered
    |> should
        equal
        [ "Fantomas could not parse bad.fs:"
          ""
          "bad.fs(3,9): error FS0583: Unmatched '('"
          ""
          "1 | module A"
          "2 | "
          "3 | let a = (1 + 2"
          "  |         ^"
          "4 | "
          "5 | let b = 2"
          "" ]

[<Test>]
let ``the caret goes on the first error, not on a warning that sorts ahead of it`` () =
    let rendered =
        Diagnostics.renderParseFailure
            "bad.fs"
            source
            [ warning 1104 "Reserved" (1, 0) (1, 6)
              error 583 "Unmatched '('" (3, 8) (3, 9) ]

    rendered |> should contain "3 | let a = (1 + 2"
    rendered |> should contain "  |         ^"

[<Test>]
let ``the window is clipped at the start and the end of the file`` () =
    let rendered =
        Diagnostics.renderParseFailure "bad.fs" "let a = 1\n" [ error 10 "Unexpected" (1, 0) (1, 3) ]

    lines rendered
    |> should
        equal
        [ "Fantomas could not parse bad.fs:"
          ""
          "bad.fs(1,1): error FS0010: Unexpected"
          ""
          "1 | let a = 1"
          "  | ^^^"
          "2 | "
          "" ]

[<Test>]
let ``tabs are expanded in the line and under the caret, so the two stay aligned`` () =
    let rendered =
        Diagnostics.renderParseFailure
            "bad.fs"
            "module A\n\n\tlet a = (1\n"
            [ error 583 "Unmatched '('" (3, 9) (3, 10) ]

    lines rendered
    |> should
        equal
        [ "Fantomas could not parse bad.fs:"
          ""
          "bad.fs(3,10): error FS0583: Unmatched '('"
          ""
          "1 | module A"
          "2 | "
          "3 |     let a = (1"
          "  |             ^"
          "4 | "
          "" ]

[<Test>]
let ``a tab inside the range widens the caret run by as much as it widened the line`` () =
    let rendered =
        Diagnostics.renderParseFailure "bad.fs" "module A\n\nlet a\t= 1\n" [ error 10 "Unexpected" (3, 3) (3, 7) ]

    lines rendered
    |> should
        equal
        [ "Fantomas could not parse bad.fs:"
          ""
          "bad.fs(3,4): error FS0010: Unexpected"
          ""
          "1 | module A"
          "2 | "
          "3 | let a    = 1"
          "  |    ^^^^^^^"
          "4 | "
          "" ]

[<Test>]
let ``a range that runs past its first line is underlined to the end of that line`` () =
    let rendered =
        Diagnostics.renderParseFailure "bad.fs" source [ error 3118 "Incomplete" (3, 0) (5, 9) ]

    rendered |> should contain "  | ^^^^^^^^^^^^^^"

[<Test>]
let ``a range beyond the end of the file leaves the snippet out rather than throwing`` () =
    let rendered =
        Diagnostics.renderParseFailure "bad.fs" "let a = 1\n" [ error 10 "Unexpected" (40, 0) (40, 3) ]

    lines rendered
    |> should
        equal
        [ "Fantomas could not parse bad.fs:"
          ""
          "bad.fs(40,1): error FS0010: Unexpected"
          "" ]

[<Test>]
let ``without source there is no snippet`` () =
    let rendered =
        Diagnostics.renderParseFailure "bad.fs" "" [ error 583 "Unmatched '('" (3, 8) (3, 9) ]

    lines rendered |> List.length |> should equal 4

[<Test>]
let ``an exception that is not a parse failure is not this module's to describe`` () =
    Diagnostics.describeParseFailure "bad.fs" (fun () -> source) (exn "boom")
    |> should equal None
