namespace Fantomas.Core

open Fantomas.FCS.Parse
open Fantomas.FCS.Text

/// A `//` or `(* *)` comment of the source: where it is, and its text.
[<NoComparison>]
type SourceComment =
    {
        /// Where the comment is in the source. Lines count from one and columns from zero, as
        /// everywhere in `Fantomas.FCS`.
        Range: range
        /// The comment as the source has it, without trailing whitespace or carriage returns. The
        /// comments sharing a line with no code on it are one, with the range of them all.
        Text: string
    }

/// The checks formatting runs on its own result before handing it back, in any combination. Each
/// runs as asked and reports only its own kind of `ValidationIssue`; none changes what another does.
/// No check runs when the result is the source apart from trailing whitespace: formatting changed
/// nothing there to check.
///
/// The comments checked are the `//` and `(* *)` comments the parser records. XML doc comments
/// (`///`) are not among them: the parser keeps those with the declaration they document, and no
/// check reads them.
[<System.Flags>]
type Validations =
    /// Check nothing.
    | None = 0
    /// Look for the text of every comment of the source in the result, in source order, without
    /// parsing it. Next to free. Reports `MissingComment`.
    | CommentSearch = 1
    /// Parse the result under each of its define combinations. Reports `NotValidFSharp`.
    | Parse = 2
    /// Parse the result and compare its comments and directives with those of the source, under
    /// each define combination of the source the result parses under without errors. Reports
    /// `CommentsChanged` and `DirectivesChanged`, and `CheckFailed` when reading the result's trivia
    /// fails.
    | TriviaComparison = 4
    /// Format the result again, when it has no parse errors under any of its define combinations.
    /// Reports `NotIdempotent`, and `CheckFailed` when formatting it fails.
    | Idempotency = 8
    /// Every check. A check added above is added here too.
    | All = 15

/// Something wrong with a formatted result, found by one of the `Validations`. Each is a bug in
/// Fantomas, and the result should not replace the source. Source that is not valid F# never gets
/// this far: formatting it raises `ParseException`, or `DefineParseException` when it has
/// conditional directives.
///
/// `defines` is the define combination a case was found under, empty for source without
/// conditional directives.
[<RequireQualifiedAccess; NoComparison>]
type ValidationIssue =
    /// `CommentSearch`: a comment of the source the result does not have.
    | MissingComment of comment: SourceComment
    /// `Parse`: what makes the result not valid F# under `defines`: every error, and every warning
    /// Fantomas does not tolerate. Positions are in the result, not in the source.
    | NotValidFSharp of defines: string list * diagnostics: FSharpParserDiagnostic list
    /// `TriviaComparison`: the comments of the source the result lacks, and the text of those of the
    /// result the source lacks, under `defines`. A block comment in both moved between a line of its
    /// own and a line with code, which formatting does not do; a line comment may.
    | CommentsChanged of defines: string list * missing: SourceComment list * added: string list
    /// `TriviaComparison`: the conditional directives (`#if`, `#elif`, `#else`, `#endif`) and warn
    /// directives (`#nowarn`, `#warnon`) of the source the result lacks, and those of the result the
    /// source lacks, with their whitespace made single spaces. Both are empty when the result has
    /// the same directives in another order: merging the define combinations relies on that order.
    | DirectivesChanged of defines: string list * missing: string list * added: string list
    /// `Idempotency`: formatting the result gave this instead of the result.
    | NotIdempotent of formattedAgain: string
    /// `TriviaComparison` or `Idempotency`: `check` could not finish, because reading the result or
    /// formatting it again raised `error`. Fantomas failing on its own result is as much a bug as
    /// any other issue, and formatting still hands back the result.
    | CheckFailed of check: Validations * error: exn

[<NoComparison>]
type FormatResult =
    {
        /// Formatted code
        Code: string
        /// New position of the input cursor.
        /// This can be None when no cursor was passed as input or no position was resolved.
        Cursor: pos option
        /// What the `Validations` asked for found wrong with `Code`. Empty when they found nothing,
        /// and when nothing was asked.
        Issues: ValidationIssue list
    }

/// What Fantomas made of a piece of F# source when it was asked whether that source is valid.
///
/// The verdict and the reason for it together, because a caller that has to tell somebody why
/// cannot reconstruct it from a boolean, and asking twice would parse the source twice.
[<NoComparison>]
type ValidationResult =
    {
        /// The diagnostics that make the source invalid: every error, and every warning Fantomas
        /// does not tolerate. Everything the parser was willing to overlook is left out, so this is
        /// empty exactly when the source is valid rather than being all the parser had to say.
        ///
        /// When the source carries conditional directives, these come from the first define
        /// combination that failed. Every combination is parsed from the same text, so the
        /// positions are positions in that text whichever combination produced them.
        Diagnostics: FSharpParserDiagnostic list
    }

    /// Whether the source is valid F# as far as Fantomas is concerned.
    member this.IsValid: bool = List.isEmpty this.Diagnostics
