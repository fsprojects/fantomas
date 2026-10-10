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
    // Not `%04i`: a hole with a width goes through F#'s printf machinery, which Native AOT cannot
    // run, and a parse error is the one thing this has to be able to report.
    | Some number -> "FS" + number.ToString("0000", Globalization.CultureInfo.InvariantCulture)
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
// A coding agent can do the reporting: the skill shrinks the file to a minimal sample and opens the
// same issue form for the reader to review. The command is what the README and the reporting page
// give, so it is the one to copy. Said by the command line alone, to the reader at a terminal: an
// editor shows the daemon's report to somebody who did not type a command.
let agentSkill (theme: Theme) : string =
    String.Concat(
        "With a coding agent, the fantomas-report skill can do that for you: ",
        flagName theme "npx skills add fsprojects/fantomas --skill fantomas-report -g",
        "."
    )

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

// The same request when a report has more than one failure of Fantomas in it.
let reportAsBugs (theme: Theme) (evidence: string) : string =
    String.Concat(
        "These are bugs in Fantomas, not problems with your code. Please report them with ",
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

// The same complaint under several define combinations is one complaint about the same text, so
// each diagnostic is listed once, with every combination that reported it.
let groupedDiagnostics (issues: ValidationIssue list) : (FSharpParserDiagnostic * string list list) list =
    issues
    |> List.collect (fun (issue: ValidationIssue) ->
        match issue with
        | ValidationIssue.NotValidFSharp(defines, diagnostics) ->
            diagnostics
            |> List.map (fun (diagnostic: FSharpParserDiagnostic) -> diagnostic, defines)
        | _ -> []
    )
    |> List.groupBy (fun (diagnostic: FSharpParserDiagnostic, _) ->
        diagnostic.Range, diagnostic.ErrorNumber, diagnostic.Message
    )
    |> List.map (fun (_, reported: (FSharpParserDiagnostic * string list) list) ->
        // The parser can say the same thing more than once in one parse: an offside token is
        // reported again each time error recovery passes it.
        fst (List.head reported), List.map snd reported |> List.distinct
    )

let resultDiagnostics (issues: ValidationIssue list) : FSharpParserDiagnostic list =
    groupedDiagnostics issues
    |> List.map (fun (diagnostic: FSharpParserDiagnostic, _) -> diagnostic)

let lostComments (issues: ValidationIssue list) : SourceComment list =
    issues
    |> List.choose (fun (issue: ValidationIssue) ->
        match issue with
        | ValidationIssue.MissingComment comment -> Some comment
        | _ -> None
    )

// Paragraph one of the report, and the opening of the message the failure carries: what went wrong,
// in one line. The report lists the comments right after it, so there it says "these" and ends on a
// colon; the message has no room for them. Nothing in it is coloured, so it needs no theme.
// What the search knows about a comment it misses: that its text is not where it belongs in the
// result. That is a comment dropped, rewritten or printed ahead of one it followed alike, so it is
// said as that rather than as a loss.
let invalidOutputSummary (invalid: bool) (missingComments: SourceComment list) (listed: bool) : string =
    let comments: string =
        match missingComments, listed with
        | [ _ ], true -> "this comment of your file"
        | _, true -> "these comments of your file"
        | [ _ ], false -> "a comment of your file"
        | _, false -> "comments of your file"

    let ending: string = if listed then ":" else "."

    match invalid, missingComments with
    | _, [] -> "Your file is unchanged because the formatted result is not valid F#."
    | false, _ -> $"Your file is unchanged because Fantomas cannot find %s{comments} in the formatted result%s{ending}"
    | true, _ ->
        $"Your file is unchanged because the formatted result is not valid F#, and Fantomas cannot find %s{comments} in it%s{ending}"

// Asked in one place so that the report and the message cannot come to send a reader after
// different things. The file, because it is the input that reproduces this and the only part of it
// the reader still has: the output that failed is thrown away.
let invalidOutputReportRequest (theme: Theme) : string = reportAsBug theme "the file"

let invalidOutputExplanation (theme: Theme) (issues: ValidationIssue list) : string =
    let summary: string =
        invalidOutputSummary (not (List.isEmpty (resultDiagnostics issues))) (lostComments issues) false

    String.Concat(summary, "\n\n", invalidOutputReportRequest theme)

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

// Whether text has a conditional directive, which is what gives it more than the one combination
// without defines. A directive starts its line, after whitespace at most. A line of a string that
// looks like one only makes a report name the combination without defines.
let hasConditionalDirectives (text: string) : bool =
    Text.RegularExpressions.Regex.IsMatch(text, @"^[ \t]*#if\b", Text.RegularExpressions.RegexOptions.Multiline)

// The define combinations a diagnostic of the output was reported under, when the output has
// conditional directives. An empty list is the combination without defines then, and is named,
// since it is one branch of several. Without directives it is the only combination and says nothing.
let reportedUnder (theme: Theme) (hasDirectives: bool) (combinations: string list list) : string =
    match combinations with
    | [ [] ] when not hasDirectives -> ""
    | combinations ->

    combinations
    |> List.map (fun (defines: string list) ->
        match defines with
        | [] -> "with no defines"
        | defines ->

        let names: string = defines |> List.map (placeholder theme) |> String.concat ", "
        $"with %s{names} defined"
    )
    |> String.concat ", "
    |> sprintf " (%s)"

// A comment can span lines, so each is its own indented block rather than an item in a sentence.
let commentLines (theme: Theme) (comments: string list) : string list =
    [
        for comment in comments do
            for line in comment.Split('\n') do
                yield "    " + commentText theme (expandTabs (line.TrimEnd('\r')))
    ]

// Each lost comment as the file has it, beside the numbers of the lines it is on in the file, in the
// gutter a snippet has. These are positions in the file, unlike those of the output below them. The
// first line is put back at its column, so the lines after it line up as they do in the file.
let missingCommentLines (theme: Theme) (comments: SourceComment list) : string list =
    let lastLine: int =
        comments
        |> List.fold (fun (last: int) (comment: SourceComment) -> max last comment.Range.EndLine) 0

    let gutter: int = String.length (string<int> lastLine)

    [
        for comment in comments do
            for index, line in Array.indexed (comment.Text.Split('\n')) do
                let number: string = (string<int>(comment.Range.StartLine + index)).PadLeft(gutter)

                let text: string =
                    if index = 0 then
                        String.Concat(String(' ', comment.Range.StartColumn), line)
                    else
                        line

                yield String.Concat(muted theme (String.Concat(number, " |")), " ", commentText theme (expandTabs text))
    ]

// What the parser said about output Fantomas refused, and the output around it. Without this the
// reader is told that something was wrong with a file they cannot see and left to find it by running
// again with `--force` and reading the result. With it they have the line to cut a small
// reproduction from, which is what a report needs and what nobody can produce from prose.
//
// Said out loud that these lines are the output. They look exactly like the lines of the file and
// they are not: nothing else Fantomas prints a snippet of is anything but the source.
let outputParserLines (theme: Theme) (output: string) (issues: ValidationIssue list) : string list =
    let grouped: (FSharpParserDiagnostic * string list list) list =
        groupedDiagnostics issues
        |> List.sortBy (fun (diagnostic: FSharpParserDiagnostic, _) -> position diagnostic)

    match grouped with
    | [] -> []
    | grouped ->

    let ordered: FSharpParserDiagnostic list =
        List.map (fun (diagnostic: FSharpParserDiagnostic, _) -> diagnostic) grouped

    [
        yield "This is what the parser made of the formatted result. The lines below are that result, not your file."
        yield ""
        for diagnostic, combinations in grouped do
            yield
                String.Concat(
                    outputHeadline theme diagnostic,
                    reportedUnder theme (hasConditionalDirectives output) combinations
                )
        yield! snippetFor theme output (caretTarget ordered)
    ]

let renderInvalidOutput (theme: Theme) (file: string) (output: string) (issues: ValidationIssue list) : string =
    let missingComments: SourceComment list = lostComments issues
    let parserLines: string list = outputParserLines theme output issues

    let diagnosticLines: string list =
        match parserLines with
        | [] -> []
        | lines -> "" :: lines

    [
        yield $"%s{link theme file} could not be formatted by Fantomas:"
        yield ""
        yield invalidOutputSummary (not (List.isEmpty parserLines)) missingComments true

        if not (List.isEmpty missingComments) then
            yield ""
            yield! missingCommentLines theme missingComments
        yield! diagnosticLines
        yield ""
        yield invalidOutputReportRequest theme
        yield ""
    ]
    |> String.concat "\n"

let directiveLine (theme: Theme) (heading: string) (directives: string list) : string list =
    match directives with
    | [] -> []
    | directives ->

    let listed: string =
        directives |> List.map (placeholder theme) |> String.concat ", "

    [ ""; $"%s{heading} %s{listed}" ]

// `from` without one occurrence of each of `taken`.
let without (from: string list) (taken: string list) : string list =
    taken
    |> List.fold
        (fun (left: string list) (text: string) ->
            match List.tryFindIndex ((=) text) left with
            | Some index -> List.removeAt index left
            | None -> left
        )
        from

// A comment the result has with only its whitespace changed is the same comment, rewritten. Each
// missing comment is paired with the first such comment the result has added that is not paired yet,
// so it is shown as before and after rather than as one comment lost and an unrelated one added.
let rewrittenComments (missing: SourceComment list) (added: string list) : (SourceComment * string) list =
    let sameWords (text: string) : string =
        Text.RegularExpressions.Regex.Replace(text, @"\s+", " ").Trim()

    missing
    |> List.fold
        (fun (pairs: (SourceComment * string) list) (comment: SourceComment) ->
            match
                without added (List.map snd pairs)
                |> List.tryFind (fun (text: string) -> sameWords text = sameWords comment.Text)
            with
            | Some text -> pairs @ [ comment, text ]
            | None -> pairs
        )
        []

let sameComment (comment: SourceComment) (other: SourceComment) : bool = comment.Range.Start = other.Range.Start

// The comments of the file that are not in the result, the comments the result has added, and the
// comments in both with their whitespace changed, as text.
let commentChangeLines (theme: Theme) (issues: ValidationIssue list) : string list =
    // Each comment is shown once, the first comparison standing for what may be one per define
    // combination.
    let (comparedAway: SourceComment list), (newComments: string list) =
        issues
        |> List.tryPick (fun (issue: ValidationIssue) ->
            match issue with
            | ValidationIssue.CommentsChanged(_, missing, added) -> Some(missing, added)
            | _ -> None
        )
        |> Option.defaultValue ([], [])

    let missingComments: SourceComment list =
        lostComments issues @ comparedAway
        |> List.distinctBy (fun (comment: SourceComment) -> comment.Range.Start)
        |> List.sortBy (fun (comment: SourceComment) -> comment.Range.StartLine, comment.Range.StartColumn)

    let rewritten: (SourceComment * string) list =
        rewrittenComments missingComments newComments

    let lost: SourceComment list =
        missingComments
        |> List.filter (fun (comment: SourceComment) -> not (List.exists (fst >> sameComment comment) rewritten))

    let added: string list = without newComments (List.map snd rewritten)

    // The comparison read the result and found the comment gone. The search alone only knows its
    // text is not where it belongs, which a comment printed ahead of one it followed is too.
    let (confirmed: SourceComment list), (searchedFor: SourceComment list) =
        lost
        |> List.partition (fun (comment: SourceComment) -> List.exists (sameComment comment) comparedAway)

    let block (heading: string) (comments: SourceComment list) : string list =
        match comments with
        | [] -> []
        | comments -> [ ""; heading; ""; yield! missingCommentLines theme comments ]

    let lostHeading: string =
        if List.length confirmed = 1 then
            "This comment of your file is not in the formatted result:"
        else
            "These comments of your file are not in the formatted result:"

    let searchedForHeading: string =
        if List.length searchedFor = 1 then
            "Fantomas cannot find this comment of your file in the formatted result:"
        else
            "Fantomas cannot find these comments of your file in the formatted result:"

    let addedHeading: string =
        let what: string =
            if List.length added = 1 then
                "this comment"
            else
                "these comments"

        let instead: string = if List.isEmpty lost then "" else " instead"
        $"The formatted result has %s{what} added%s{instead}:"

    [
        for before, after in rewritten do
            yield ""
            yield "Formatting changes the whitespace inside this comment of your file:"
            yield ""
            yield! missingCommentLines theme [ before ]
            yield ""
            yield "The formatted result has it like this:"
            yield ""
            yield! commentLines theme [ after ]

        yield! block lostHeading confirmed
        yield! block searchedForHeading searchedFor

        if not (List.isEmpty added) then
            yield ""
            yield addedHeading
            yield ""
            yield! commentLines theme added
    ]

// What the first comparison that found the directives changed found, as text: a directive is the
// same under every combination.
let directiveChangeLines (theme: Theme) (issues: ValidationIssue list) : string list =
    issues
    |> List.tryPick (fun (issue: ValidationIssue) ->
        match issue with
        | ValidationIssue.DirectivesChanged(_, missing, added) -> Some(missing, added)
        | _ -> None
    )
    |> Option.map (fun (missing: string list, added: string list) ->
        match missing, added with
        | [], [] ->
            [
                ""
                "The formatted result has the directives of your file in a different order."
            ]
        | missing, added ->
            [
                yield! directiveLine theme "These directives of your file are not in the formatted result:" missing
                yield! directiveLine theme "The formatted result has these directives added:" added
            ]
    )
    |> Option.defaultValue []

// What happened to the file, rather than which check saw it or under which defines: the line numbers
// already say where a comment is.
let triviaChangeLines (theme: Theme) (issues: ValidationIssue list) : string list =
    let failed: string list =
        issues
        |> List.tryPick (fun (issue: ValidationIssue) ->
            match issue with
            | ValidationIssue.CheckFailed(_, error) ->
                Some
                    [
                        ""
                        $"Comparing the comments and directives of your file with those of the formatted result failed: %s{error.Message}"
                    ]
            | _ -> None
        )
        |> Option.defaultValue []

    commentChangeLines theme issues @ directiveChangeLines theme issues @ failed
