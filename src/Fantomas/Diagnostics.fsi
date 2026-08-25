module Fantomas.Diagnostics

open Fantomas.FCS.Parse

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
val renderParseFailure: file: string -> source: string -> diagnostics: FSharpParserDiagnostic list -> string

/// Render `error` as the text to report for `file`, when it is a parse failure and therefore has
/// diagnostics worth positioning. Anything else is not this module's to describe and comes back
/// as `None`.
///
/// `source` yields the text those diagnostics were produced from, and is called only once `error`
/// turns out to be a parse failure. A caller that has to read a file to produce it therefore does
/// not read one for a failure that has nothing to do with parsing.
val describeParseFailure: file: string -> source: (unit -> string) -> error: exn -> string option

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
    file: string -> source: string -> verbose: bool -> violation: Fantomas.Core.InvariantViolationException -> string

/// Render `error` as the text to report for `file`, when it is an invariant violation and therefore
/// points at a construct Fantomas could not model. Anything else comes back as `None`.
///
/// `source` yields the text being formatted, and is called only once `error` turns out to be an
/// invariant violation.
val describeInvariantViolation: file: string -> source: (unit -> string) -> verbose: bool -> error: exn -> string option
