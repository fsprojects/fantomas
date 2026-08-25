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

let renderParseFailure
    (theme: Theme)
    (file: string)
    (source: string)
    (diagnostics: FSharpParserDiagnostic list)
    : string
    =
    let ordered = List.sortBy position diagnostics

    let snippetLines =
        if String.IsNullOrEmpty source then
            []
        else
            // The caret goes on the first error rather than the first diagnostic. A warning can
            // sort ahead of the error that stopped the parse, and it is not why the file failed.
            let firstError =
                ordered
                |> List.tryPick (fun diagnostic ->
                    match diagnostic.Severity, diagnostic.Range with
                    | FSharpDiagnosticSeverity.Error, Some range -> Some range
                    | _ -> None
                )

            match firstError with
            | None -> []
            | Some range ->
                let lines = source.Replace("\r\n", "\n").Split('\n')

                match snippet theme lines range with
                | [] -> []
                | snippetLines -> "" :: snippetLines

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

    let snippetLines: string list =
        if String.IsNullOrEmpty source then
            []
        else
            let lines: string array = source.Replace("\r\n", "\n").Split('\n')

            match snippet theme lines violation.Range with
            | [] -> []
            | snippetLines -> "" :: snippetLines

    // The dump of the syntax tree node is what tells a maintainer which parser shape went
    // unhandled, and it is noise to everyone else, so it is shown only when asked for.
    let syntaxNodeLines: string list =
        if not verbose || String.IsNullOrWhiteSpace violation.SyntaxNode then
            []
        else
            [ ""; "Syntax tree node:"; "" ]
            @ List.ofArray (violation.SyntaxNode.Split('\n'))

    // The two places to report it are somewhere the reader can go, which is the one thing colour
    // marks in prose, so they are coloured as the links they are and the sentence around them is
    // left alone.
    let reportIt: string =
        String.Concat(
            "This is a bug in Fantomas, not a problem with your code. Please report it with the snippet above via ",
            link theme "https://fsprojects.github.io/fantomas-tools/",
            ", or at ",
            link theme "https://github.com/fsprojects/fantomas/issues/new",
            " if the file is too large for the tool to carry."
        )

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
