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
val parse: isSignature: bool -> source: ISourceText -> Async<UnderDefines<ParsedInput> array>

/// One tree as `formatDocument` formatted it, handed to `inspect` under the define combination it was
/// parsed with: the Oak printed for it, the comments and directives the parser recorded in it, and
/// the code printed for it before the merge, with where the cursor ended up in that code.
/// `SecondPass` marks a tree of formatting the result again, for `Idempotency`.
[<NoComparison; NoEquality>]
type FormattedTree =
    {
        Oak: SyntaxOak.Oak
        Trivia: Trivia.RecordedTrivia
        Code: string
        Cursor: pos option
        SecondPass: bool
    }

/// The full pipeline: parse per define combination, format each tree in parallel with the source and
/// cursor, merge the results into one when there was more than one, and run the checks of
/// `validations` on that result against the comments and directives the parser recorded, which
/// leave their findings in its `Issues`.
///
/// `inspect` is the hook for the tests: it is handed every tree right after it is printed, in that
/// tree's own task, so two may be handed over at once, and the second pass's trees too. Nothing of a
/// tree is kept past it but its code and the trivia the checks need.
val formatDocument:
    inspect: (UnderDefines<FormattedTree> -> unit) ->
    config: FormatConfig ->
    isSignature: bool ->
    source: ISourceText ->
    cursor: pos option ->
    validations: Validations ->
        Async<FormatResult>
