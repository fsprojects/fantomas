namespace Fantomas.Core

open Fantomas.FCS.Parse
open Fantomas.FCS.Text

[<NoComparison>]
type FormatResult =
    {
        /// Formatted code
        Code: string
        /// New position of the input cursor.
        /// This can be None when no cursor was passed as input or no position was resolved.
        Cursor: pos option
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
