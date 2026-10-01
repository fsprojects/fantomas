module internal Fantomas.Core.Trivia

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text
open Fantomas.Core.SyntaxOak

/// The smallest node whose range contains `range`, found by descending into the first child that
/// still contains it. This is the node trivia and selections are resolved against.
val findNodeWhereRangeFitsIn: root: Node -> range: range -> Node option

/// The text of every comment in the source, normalised, so that the comments of the input and of
/// the output can be compared to prove none was lost.
val collectCommentTextsFromAST: sourceText: ISourceText -> ast: ParsedInput -> Set<TriviaContent>

/// Attach everything the Oak does not represent, comments, blank lines and directives, to the
/// nodes it belongs with. Mutates `tree` in place and returns it.
///
/// Comments and directives come from the trivia the parser recorded; blank lines are read from the
/// source text. Each piece is attached to the smallest node whose range contains it, as
/// `ContentBefore` of the child that follows it or `ContentAfter` of the child that precedes it.
///
/// Which side wins depends on the kind of trivia. A comment after code on the same line goes after
/// the last node on that line. A comment on its own line goes before the next sibling, unless that
/// sibling is a closing bracket, or the comment is indented to the column of the previous sibling
/// while the next one is not. Blank lines before an indented comment travel with the comment.
/// Trivia outside every node is attached to the root.
///
/// The choices are illustrated in src/Fantomas.Core.Tests/TriviaAssignmentTests.fs.
val enrichTree: config: FormatConfig -> sourceText: ISourceText -> ast: ParsedInput -> tree: Oak -> Oak

/// Record the editor's cursor in the Oak so that CodePrinter can report where it ends up.
/// When the cursor sits inside a `SingleTextNode`, the node remembers it and the printer reports
/// the same offset into the printed text. Otherwise a `Cursor` trivia is attached to the smallest
/// node around it, and the printer reports the position after that node. Mutates `tree` in place.
/// The end-to-end behaviour is in src/Fantomas.Core.Tests/CursorTests.fs.
val insertCursor: tree: Oak -> cursor: pos -> Oak
