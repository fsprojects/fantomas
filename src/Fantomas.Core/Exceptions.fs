namespace Fantomas.Core

open System
open Fantomas.FCS.Diagnostics
open Fantomas.FCS.Parse
open Fantomas.FCS.Text

// The ways formatting can fail, and nothing else. Every one of them derives from `FormatException`,
// which the CLI matches on to decide what to print, so a failure that reaches a caller with nothing
// better to say still has a message rather than an empty string.
//
// Each carries what went wrong as data and composes its message in an override of `Message` rather
// than in the call to the base constructor, which is why each hands the base an empty one. The
// message is then built only if something asks for it, and on the usual path nothing does: every
// reporter in Fantomas renders from the data and reaches for the message only as a last resort.
//
// They live apart from `FormatConfig` because they share nothing with it: a setting and a failure
// are not the same subject, and a reader looking for one of these would not think to open a file
// named for the other. `FormatConfig` does raise `FormatException` for a setting it will not accept,
// which is why this file compiles first.

/// Raised when Fantomas encounters a problem during formatting.
type FormatException(msg: string) =
    inherit Exception(msg)

/// Raised when the F# parser produces errors for source code without conditional directives.
type ParseException(diagnostics: FSharpParserDiagnostic list) =
    inherit FormatException(String.Empty)

    /// Every diagnostic the parser produced, warnings included. A warning is often what explains the
    /// error, and which of them are worth showing is the reporter's decision rather than this type's.
    member _.Diagnostics = diagnostics

    override _.Message =
        // One line naming an error, not the `%A` dump of every record that the `exception` keyword
        // this replaces produced. Earliest by position rather than first the parser gave, so it
        // names the same error a reporter draws its caret under: in an offside cascade that is the
        // line that caused it rather than the innocent line the parser gave up on.
        let position (d: FSharpParserDiagnostic) : int * int =
            match d.Range with
            | None -> Int32.MaxValue, 0
            | Some range -> range.StartLine, range.StartColumn

        let firstError: FSharpParserDiagnostic option =
            diagnostics
            |> List.filter (fun (d: FSharpParserDiagnostic) -> d.Severity = FSharpDiagnosticSeverity.Error)
            |> List.sortBy position
            |> List.tryHead

        match firstError with
        | None -> "Fantomas could not parse the source."
        | Some error ->

        match error.Range with
        | None -> $"Fantomas could not parse the source: %s{error.Message}"
        | Some range ->
            $"Fantomas could not parse the source: %s{error.Message} at line %i{range.StartLine}, column %i{range.StartColumn + 1}."

/// Raised when Fantomas reaches a state that its own model says is impossible, for example a
/// chain whose parts do not fit the shape the transformer guarantees.
///
/// Unlike the other exceptions here, this never indicates a problem with the code being
/// formatted: it is a bug in Fantomas, or a change in how the F# parser groups expressions.
/// Failing loudly is deliberate — the alternative is silently dropping parts of the source.
type InvariantViolationException(msg: string, range: range, syntaxNode: string) =
    inherit FormatException(String.Empty)

    new(msg: string, range: range) = InvariantViolationException(msg, range, String.Empty)

    /// The invariant that was violated, on one line, without the location or the "please report"
    /// suffix. A caller that positions the violation against the source itself reports this and
    /// draws the rest from `Range`.
    member _.Invariant = msg

    /// The source range of the construct that triggered the violation.
    member _.Range = range

    /// The syntax tree node that triggered the violation, dumped in full, or an empty string when
    /// there is none to show. This is what a maintainer triaging the issue needs and what whoever
    /// filed it does not, so it is kept off the message and reported at detailed verbosity.
    member _.SyntaxNode = syntaxNode

    override _.Message =
        $"%s{msg}\nAt line %i{range.StartLine}, column %i{range.StartColumn} in %s{range.FileName}.\nThis is a bug in Fantomas. Please report it via https://fsprojects.github.io/fantomas-tools/"

/// Raised when one or more conditional compilation define combinations produce invalid syntax trees.
type DefineParseException(combinations: string list) =
    inherit FormatException(String.Empty)

    /// The define combinations that failed to parse.
    member _.Combinations = combinations

    override _.Message =
        let joined: string = combinations |> String.concat ", "
        $"Parsing failed for define combination(s): %s{joined}."
