module Fantomas.Diagnostics

open System
open Fantomas.Core
open Fantomas.FCS.Diagnostics
open Fantomas.FCS.Parse
open Fantomas.FCS.Text
open Fantomas.Theme

// How many lines of the file are shown either side of the one the caret points at.
let contextLines: int = 2

// A tab is one column to the parser and one character in the file, but any number of columns on
// screen. Both the line and the caret indent are expanded the same way, so the caret stays under
// the token whatever the terminal would have done with the tab.
let tabStop: string = "    "

let expandTabs (line: string) : string = line.Replace("\t", tabStop)

let severityText (diagnostic: FSharpParserDiagnostic) : string =
    match diagnostic.Severity with
    | FSharpDiagnosticSeverity.Error -> "error"
    | FSharpDiagnosticSeverity.Warning -> "warning"
    | FSharpDiagnosticSeverity.Info
    | FSharpDiagnosticSeverity.Hidden -> "info"

// FS0000 is what the compiler prints when it has no number to give, so an absent one is not
// special cased into a different shape.
let errorNumber (diagnostic: FSharpParserDiagnostic) : string =
    match diagnostic.ErrorNumber with
    | Some number -> $"FS%04i{number}"
    | None -> "FS0000"

// The word carries the same weight the exit codes give it: an error is what ends the run, a warning
// is something to look at that did not.
let severityColour (theme: Theme) (diagnostic: FSharpParserDiagnostic) : string =
    let word: string = severityText diagnostic

    match diagnostic.Severity with
    | FSharpDiagnosticSeverity.Error -> negative theme word
    | FSharpDiagnosticSeverity.Warning -> attention theme word
    | FSharpDiagnosticSeverity.Info
    | FSharpDiagnosticSeverity.Hidden -> muted theme word

// The range carries `tmp.fsx` or `tmp.fsi`, the name the parser was handed, so the path has to
// come from the caller. One line per diagnostic, which means the message cannot keep newlines.
//
// The colour goes on the three parts a reader picks the line out by: where it happened, how bad it
// is, and which diagnostic it is. The message itself is prose and stays plain, the way the help
// page leaves its descriptions plain beside a coloured flag.
let headline (theme: Theme) (file: string) (diagnostic: FSharpParserDiagnostic) : string =
    let message = diagnostic.Message.Replace("\r\n", " ").Replace("\n", " ")

    let location: string =
        match diagnostic.Range with
        | None -> file
        | Some range -> $"%s{file}(%i{range.StartLine},%i{range.StartColumn + 1})"

    let severity: string = severityColour theme diagnostic
    let number: string = placeholder theme (errorNumber diagnostic)

    $"%s{link theme location}: %s{severity} %s{number}: %s{message}"

let position (diagnostic: FSharpParserDiagnostic) : int * int =
    match diagnostic.Range with
    | Some range -> range.StartLine, range.StartColumn
    | None -> Int32.MaxValue, 0

let caretRun (line: string) (range: range) : string * string =
    let startColumn = min range.StartColumn line.Length

    let endColumn =
        // A range that runs past the end of its first line is underlined to the end of that line;
        // the following lines are in the snippet anyway.
        if range.EndLine = range.StartLine then
            min range.EndColumn line.Length
        else
            line.Length

    // Both the indent and the run are measured on the expanded text, so a tab inside the range
    // widens the carets by as much as it widened the line.
    let indent = (expandTabs (line.Substring(0, startColumn))).Length

    let width =
        max 1 (expandTabs (line.Substring(startColumn, endColumn - startColumn))).Length

    String(' ', indent), String('^', width)

// The gutter is scaffolding and carries nothing a reader has to take in, so it is dimmed; the
// source between the gutters is the file's own text and is left exactly as it is. The carets are
// the one thing on the line that is Fantomas speaking, and they say where.
//
// Padded before it is coloured, because padding counts characters and an escape sequence is
// characters that take no width on screen.
let snippet (theme: Theme) (lines: string array) (range: range) : string list =
    if range.StartLine < 1 || range.StartLine > lines.Length then
        []
    else
        let firstLine = max 1 (range.StartLine - contextLines)
        let lastLine = min lines.Length (range.StartLine + contextLines)
        let gutter = String.length (string<int> lastLine)

        let blankGutter: string = muted theme (String.Concat(String(' ', gutter), " |"))

        [
            for number in firstLine..lastLine do
                let lineNumber: string = (string<int> number).PadLeft(gutter)
                let numberedGutter: string = muted theme (String.Concat(lineNumber, " |"))
                yield String.Concat(numberedGutter, " ", expandTabs lines.[number - 1])

                if number = range.StartLine then
                    let indent, carets = caretRun lines.[number - 1] range
                    yield String.Concat(blankGutter, " ", indent, negative theme carets)
        ]

// Where to draw the caret. The first error by position, since in an offside cascade that is the
// line that caused it rather than the innocent line the parser gave up on. Falling back to the
// first diagnostic that has a range at all, for a report whose diagnostics are warnings that
// Fantomas will not tolerate and which therefore has no error to point at.
let caretTarget (ordered: FSharpParserDiagnostic list) : range option =
    let firstError: range option =
        ordered
        |> List.tryPick (fun diagnostic ->
            match diagnostic.Severity, diagnostic.Range with
            | FSharpDiagnosticSeverity.Error, Some range -> Some range
            | _ -> None
        )

    match firstError with
    | Some range -> Some range
    | None -> List.tryPick (fun (diagnostic: FSharpParserDiagnostic) -> diagnostic.Range) ordered

// The snippet as a section of a report: a blank line and then the lines, or nothing at all when
// there is no source to draw from and no range to draw at. Every report here places it the same
// way, so where the blank line goes is decided once.
let snippetFor (theme: Theme) (source: string) (target: range option) : string list =
    match target with
    | None -> []
    | Some range ->

    if String.IsNullOrEmpty source then
        []
    else

    let lines: string array = source.Replace("\r\n", "\n").Split('\n')

    match snippet theme lines range with
    | [] -> []
    | snippetLines -> "" :: snippetLines

let renderParseFailure
    (theme: Theme)
    (file: string)
    (source: string)
    (diagnostics: FSharpParserDiagnostic list)
    : string
    =
    let ordered = List.sortBy position diagnostics

    // The caret goes on the first error rather than the first diagnostic. A warning can sort ahead
    // of the error that stopped the parse, and it is not why the file failed.
    let snippetLines: string list = snippetFor theme source (caretTarget ordered)

    // The report ends with a blank line as well as starting with one, so that a run over several
    // files does not have one file's snippet running into the next file's header.
    [
        yield $"%s{link theme file} could not be parsed by Fantomas:"
        yield ""
        yield! List.map (headline theme file) ordered
        yield! snippetLines
        yield ""
    ]
    |> String.concat "\n"

let describeParseFailure (theme: Theme) (file: string) (source: unit -> string) (error: exn) : string option =
    match error with
    | :? ParseException as parseFailure -> Some(renderParseFailure theme file (source ()) parseFailure.Diagnostics)
    | _ -> None

// The same request, worded once, so that the two failures Fantomas has to own up to cannot come to
// ask for a report in two different ways. What differs is the evidence worth sending: one of these
// points at a construct in the file and the other at what the whole file was turned into.
//
// One place to send it. It used to name the issue tracker as well, for a file too large for the
// tool to carry, which offered a reader a choice at the moment they have least appetite for one and
// pointed half of them at the slower path. If the tool cannot take a file that size, that is the
// tool's problem to fix rather than a fork to put in front of somebody reporting a bug.
//
// The place to report it is somewhere the reader can go, which is the one thing colour marks in
// prose, so it is coloured as the link it is and the sentence around it is left alone.
let reportAsBug (theme: Theme) (evidence: string) : string =
    String.Concat(
        "This is a bug in Fantomas, not a problem with your code. Please report it with ",
        evidence,
        " via ",
        link theme "https://fsprojects.github.io/fantomas-tools/",
        "."
    )

let renderInvariantViolation
    (theme: Theme)
    (file: string)
    (source: string)
    (verbose: bool)
    (violation: InvariantViolationException)
    : string
    =
    // The range names the file the parser was handed, `tmp.fsx`, so the path comes from the caller
    // here as it does for a parse diagnostic. Naming a file that is not the one being formatted is
    // worse than saying nothing, because it reads as though Fantomas looked somewhere else.
    let headline: string =
        let location: string =
            $"%s{file}(%i{violation.Range.StartLine},%i{violation.Range.StartColumn + 1})"

        let severity: string = negative theme "error"

        $"%s{link theme location}: %s{severity}: %s{violation.Invariant}"

    let snippetLines: string list = snippetFor theme source (Some violation.Range)

    // The dump of the syntax tree node is what tells a maintainer which parser shape went
    // unhandled, and it is noise to everyone else, so it is shown only when asked for.
    let syntaxNodeLines: string list =
        if not verbose || String.IsNullOrWhiteSpace violation.SyntaxNode then
            []
        else
            [ ""; "Syntax tree node:"; "" ]
            @ List.ofArray (violation.SyntaxNode.Split('\n'))

    let reportIt: string = reportAsBug theme "the snippet above"

    [
        yield $"%s{link theme file} could not be formatted by Fantomas:"
        yield ""
        yield headline
        yield! snippetLines
        yield! syntaxNodeLines
        yield ""
        yield reportIt
        yield ""
    ]
    |> String.concat "\n"

let describeInvariantViolation
    (theme: Theme)
    (file: string)
    (source: unit -> string)
    (verbose: bool)
    (error: exn)
    : string option
    =
    match error with
    | :? InvariantViolationException as violation ->
        Some(renderInvariantViolation theme file (source ()) verbose violation)
    | _ -> None

// Paragraph one of the report, and the opening of the message the failure carries. Split out
// because the report puts the parser's own words between it and the request for a report, and the
// message does not. Nothing in it is coloured, so it needs no theme.
let invalidOutputSummary: string =
    "Fantomas formatted this file and then found that its own output did not pass validation, so the output was thrown away and your file is unchanged."

// Asked in one place so that the report and the message cannot come to send a reader after
// different things. The file, because it is the input that reproduces this and the only part of it
// the reader still has: the output that failed is thrown away.
let invalidOutputReportRequest (theme: Theme) : string = reportAsBug theme "the file"

let invalidOutputExplanation (theme: Theme) : string =
    String.Concat(invalidOutputSummary, "\n\n", invalidOutputReportRequest theme)

// No position, which is the one thing this drops from the shape every other diagnostic here is
// printed in. A position is somewhere to go, and there is nowhere to go: the output it counts lines
// into is thrown away and was never written. `src/A.fs(4708,25)` would be worse than useless, since
// an editor turns it into a link to line 4708 of the input, which is not the line it means. The
// carets below are what says where, and they say it by pointing at the line itself.
let outputHeadline (theme: Theme) (diagnostic: FSharpParserDiagnostic) : string =
    let message: string = diagnostic.Message.Replace("\r\n", " ").Replace("\n", " ")
    let severity: string = severityColour theme diagnostic
    let number: string = placeholder theme (errorNumber diagnostic)

    $"%s{severity} %s{number}: %s{message}"

let renderInvalidOutput
    (theme: Theme)
    (file: string)
    (output: string)
    (diagnostics: FSharpParserDiagnostic list)
    : string
    =
    let ordered: FSharpParserDiagnostic list = List.sortBy position diagnostics

    // What the parser said, and the output around it. Without this the reader is told that
    // something was wrong with a file they cannot see and left to find it by running again with
    // `--force` and reading the result. With it they have the line to cut a small reproduction
    // from, which is what a report needs and what nobody can produce from prose.
    //
    // Said out loud that these lines are the output. They look exactly like the lines of the file
    // and they are not: nothing else Fantomas prints a snippet of is anything but the source.
    let diagnosticLines: string list =
        if List.isEmpty ordered then
            []
        else
            [
                yield ""
                yield "This is what the parser made of that output. The lines below are the output, not your file."
                yield ""
                yield! List.map (outputHeadline theme) ordered
                yield! snippetFor theme output (caretTarget ordered)
            ]

    [
        yield $"%s{link theme file} could not be formatted by Fantomas:"
        yield ""
        yield invalidOutputSummary
        yield! diagnosticLines
        yield ""
        yield invalidOutputReportRequest theme
        yield ""
    ]
    |> String.concat "\n"
