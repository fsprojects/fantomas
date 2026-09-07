[<RequireQualifiedAccess>]
module internal Fantomas.Core.CodeFormatterImpl

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text

val getSourceText: source: string -> ISourceText

/// Format one abstract syntax tree using the given config.
/// With `sourceText` the Oak keeps the original spelling of literals and is enriched with trivia;
/// without it, comments and blank lines are gone and literals are re-rendered from their values.
/// `cursor` is inserted into the Oak before printing and reported back in the result.
val formatAST:
    ast: ParsedInput -> sourceText: ISourceText option -> config: FormatConfig -> cursor: pos option -> FormatResult

/// Parse the source once per define combination. A source without conditional directives yields
/// a single tree with the empty combination. Raises `FormatException` when any parse has an
/// invalidating diagnostic.
val parse: isSignature: bool -> source: ISourceText -> Async<(ParsedInput * DefineCombination) array>

/// The full pipeline: parse per define combination, format each tree in parallel with the source
/// and cursor, and merge the results into one when there was more than one.
val formatDocument:
    config: FormatConfig -> isSignature: bool -> source: ISourceText -> cursor: pos option -> Async<FormatResult>
