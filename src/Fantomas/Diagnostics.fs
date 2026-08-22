module Fantomas.Diagnostics

open System
open Fantomas.Core
open Fantomas.FCS.Diagnostics
open Fantomas.FCS.Parse
open Fantomas.FCS.Text

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

// The range carries `tmp.fsx` or `tmp.fsi`, the name the parser was handed, so the path has to
// come from the caller. One line per diagnostic, which means the message cannot keep newlines.
let headline (file: string) (diagnostic: FSharpParserDiagnostic) : string =
    let message = diagnostic.Message.Replace("\r\n", " ").Replace("\n", " ")

    match diagnostic.Range with
    | Some range ->
        $"%s{file}(%i{range.StartLine},%i{range.StartColumn + 1}): %s{severityText diagnostic} %s{errorNumber diagnostic}: %s{message}"
    | None -> $"%s{file}: %s{severityText diagnostic} %s{errorNumber diagnostic}: %s{message}"

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
    let width = max 1 (expandTabs (line.Substring(startColumn, endColumn - startColumn))).Length
    String(' ', indent), String('^', width)

let snippet (lines: string array) (range: range) : string list =
    if range.StartLine < 1 || range.StartLine > lines.Length then
        []
    else
        let firstLine = max 1 (range.StartLine - contextLines)
        let lastLine = min lines.Length (range.StartLine + contextLines)
        let gutter = String.length (string<int> lastLine)

        [ for number in firstLine..lastLine do
              let lineNumber = (string<int> number).PadLeft(gutter)
              yield $"%s{lineNumber} | %s{expandTabs lines.[number - 1]}"

              if number = range.StartLine then
                  let indent, carets = caretRun lines.[number - 1] range
                  yield $"%s{String(' ', gutter)} | %s{indent}%s{carets}" ]

let renderParseFailure (file: string) (source: string) (diagnostics: FSharpParserDiagnostic list) : string =
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
                    | _ -> None)

            match firstError with
            | None -> []
            | Some range ->
                let lines = source.Replace("\r\n", "\n").Split('\n')

                match snippet lines range with
                | [] -> []
                | snippetLines -> "" :: snippetLines

    // The report ends with a blank line as well as starting with one, so that a run over several
    // files does not have one file's snippet running into the next file's header.
    [ yield $"Fantomas could not parse %s{file}:"
      yield ""
      yield! List.map (headline file) ordered
      yield! snippetLines
      yield "" ]
    |> String.concat "\n"

let describeParseFailure (file: string) (source: string) (error: exn) : string option =
    match error with
    | ParseException diagnostics -> Some(renderParseFailure file source diagnostics)
    | _ -> None
