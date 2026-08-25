module Fantomas.Diagnostics

open Fantomas.FCS.Parse
open Fantomas.Theme

/// The word for a diagnostic's severity: `error`, `warning` or `info`. Every report says it the
/// same way, whether it is writing a line of text or a field of a document.
val severityText: diagnostic: FSharpParserDiagnostic -> string

/// The diagnostic's number in the `FSnnnn` form the compiler prints. A diagnostic that carries no
/// number is `FS0000`, which is what the compiler prints when it has none to give.
val errorNumber: diagnostic: FSharpParserDiagnostic -> string

/// Render the diagnostics of a parse failure as text a person and a machine can both read.
///
/// The first line names Fantomas and the file, because the parser Fantomas vendors can lag the
/// compiler the caller builds with, and a bare `error FS0010` would read as though the compiler
/// had rejected code it in fact accepts.
///
/// Every diagnostic then gets one MSBuild style line, `path(line,column): severity FSnnnn: message`,
/// ordered by position rather than in the order the parser produced them. Columns are one based,
/// matching what the F# compiler prints for the same file. Warnings are included: a parse failure
/// carries every diagnostic the parser produced, and a warning is often what explains the error.
///
/// Below those comes a snippet of `source` with two lines of context either side, and a caret run
/// under the offending range. It is drawn for the earliest error, which in an offside cascade is
/// the line that caused the cascade rather than the innocent line the parser stopped at. Pass an
/// empty string for `source` to leave the snippet out.
///
/// `theme` decides how much of it is coloured: the path and each diagnostic's position as a place
/// the reader can go, the severity as the outcome it is, the error number and the gutter as the
/// scaffolding they are, and the carets red. The message and the source between the gutters are
/// left exactly as they are. `Theme.plain` renders the same report as text alone, which is what a
/// redirected stream and the daemon get.
val renderParseFailure:
    theme: Theme -> file: string -> source: string -> diagnostics: FSharpParserDiagnostic list -> string

/// Render `error` as the text to report for `file`, when it is a parse failure and therefore has
/// diagnostics worth positioning. Anything else is not this module's to describe and comes back
/// as `None`.
///
/// `source` yields the text those diagnostics were produced from, and is called only once `error`
/// turns out to be a parse failure. A caller that has to read a file to produce it therefore does
/// not read one for a failure that has nothing to do with parsing.
val describeParseFailure: theme: Theme -> file: string -> source: (unit -> string) -> error: exn -> string option

/// Render an invariant violation the same way a parse failure is rendered: one MSBuild style line
/// saying what Fantomas could not model and where, then a snippet of `source` with a caret run
/// under the construct that could not be modelled.
///
/// The position comes from the violation's range but the path comes from `file`, because the range
/// carries the name the parser was handed rather than the file being formatted.
///
/// `verbose` adds the syntax tree node the violation carries. It is what a maintainer triaging the
/// report needs and noise to whoever ran the tool, so it is not shown by default.
val renderInvariantViolation:
    theme: Theme ->
    file: string ->
    source: string ->
    verbose: bool ->
    violation: Fantomas.Core.InvariantViolationException ->
        string

/// Render `error` as the text to report for `file`, when it is an invariant violation and therefore
/// points at a construct Fantomas could not model. Anything else comes back as `None`.
///
/// `source` yields the text being formatted, and is called only once `error` turns out to be an
/// invariant violation.
val describeInvariantViolation:
    theme: Theme -> file: string -> source: (unit -> string) -> verbose: bool -> error: exn -> string option

/// What to say when formatting produced output that Fantomas itself would not accept. This is the
/// message the failure carries, so that a caller with no console to draw a report on still has the
/// whole of it in words.
///
/// It does not name the file, because nothing that shows it is short of one: a reporter puts the
/// path in front of it and the JSON document carries it as a key beside it. Naming it here is what
/// made the line read `A.fs could not be formatted: Formatting A.fs leads to invalid F# code`.
///
/// What it says is that the file was not touched, which is the first thing somebody wants to know
/// after reading that formatting produced something invalid, and that this is a bug in Fantomas and
/// where to take it, since reaching this state is never the file's fault.
val invalidOutputExplanation: theme: Theme -> string

/// Render an invalid output failure for `file`: the same header the other two reports open with,
/// what happened, what the parser said about `output`, and the request for a bug report.
///
/// `output` is what Fantomas produced and would not accept, and `diagnostics` are what it would not
/// accept about it. Without them the reader is told that something was wrong with a file they cannot
/// see, and is left to run again with `--force` and find it themselves. With them they have the line
/// to cut a small reproduction from, which is what a bug report needs and what no amount of prose
/// supplies. Pass an empty list to leave the whole section out.
///
/// Each is rendered without its position, which is where this departs from every other report here.
/// A position is somewhere to go and there is nowhere to go: `output` is thrown away and written
/// nowhere, so a line number into it is a coordinate in a buffer the reader cannot open, and
/// `path(line,column)` would be a link an editor follows to the wrong line of the right file. The
/// snippet is what says where, by pointing at the line. It is drawn for the first of them, which is
/// also the first listed, since they are ordered by position.
val renderInvalidOutput:
    theme: Theme -> file: string -> output: string -> diagnostics: FSharpParserDiagnostic list -> string
