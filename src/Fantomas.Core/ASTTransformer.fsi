module internal Fantomas.Core.ASTTransformer

open Fantomas.FCS.Text
open Fantomas.FCS.Syntax
open Fantomas.Core.SyntaxOak

/// Build the Oak for a parsed file.
///
/// `sourceText` is where the text of literals is taken from. The parser keeps the value of a string,
/// number or interpolation, not the characters that spelled it, so `1_000`, `0x10` or a verbatim
/// string cannot be recovered from the tree alone. With the source, the node carries the original
/// spelling; with `None` it carries a fallback rendered from the value. Pass `None` only when the
/// tree did not come from text, as `CodeFormatter.TransformAST` without a source does.
/// The Oak comes back without trivia. `Trivia.enrichTree` adds it, and needs the source as well.
val mkOak: sourceText: ISourceText option -> ast: ParsedInput -> Oak
