module internal Fantomas.Core.Trivia

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text
open Fantomas.Core.SyntaxOak

val findNodeWhereRangeFitsIn: root: Node -> range: range -> Node option
val enrichTree: config: FormatConfig -> sourceText: ISourceText -> ast: ParsedInput -> tree: Oak -> Oak

/// Try and insert a cursor position as Trivia inside the Oak
/// The cursor could either be inside a Node or floating around one.
val insertCursor: tree: Oak -> cursor: pos -> Oak
