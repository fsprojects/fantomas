module internal Fantomas.Core.Trivia

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text
open Fantomas.Core.SyntaxOak

/// The smallest node whose range contains `range`, found by descending into the first child that
/// still contains it. This is the node trivia and selections are resolved against.
val findNodeWhereRangeFitsIn: root: Node -> range: range -> Node option

/// The comments and directives the parser recorded when it parsed the source under one define
/// combination, within the range of the tree, read before any is assigned to the Oak. The tree of a
/// whole file covers all of them. Every directive is there under every
/// combination, but a comment in a branch inactive under these defines is not. `enrichTree` reads
/// them, assigns them and hands them back, so the checks of the formatted result compare it with
/// what formatting worked from.
[<NoComparison; NoEquality>]
type RecordedTrivia =
    {
        /// In source order, as `TriviaNode`s of their kind. The comments sharing a line with no code
        /// on it are one `CommentOnSingleLine`, with the range of them all.
        Comments: TriviaNode list
        /// The conditional directives (`#if`, `#elif`, `#else`, `#endif`), then the warn directives
        /// (`#nowarn`, `#warnon`), each as a `Directive` with its text.
        Directives: TriviaNode list
    }

/// Attach everything the Oak does not represent, comments, blank lines and directives, to the
/// nodes it belongs with. Mutates `tree` in place and returns it, with the comments and directives
/// the parser recorded.
///
/// Comments and directives are what the parser recorded in `ast`, read before any is attached and
/// returned beside the tree; blank lines are read from the source text. Each piece is attached to the
/// smallest node whose range contains it, as `ContentBefore` of the child that follows it or
/// `ContentAfter` of the child that precedes it.
///
/// Which side wins depends on the kind of trivia. A comment after code on the same line goes after
/// the last node on that line. A comment on its own line goes before the next sibling, unless that
/// sibling is a closing bracket, or the comment is indented to the column of the previous sibling
/// while the next one is not. Blank lines before an indented comment travel with the comment.
/// Trivia outside every node is attached to the root.
///
/// The choices are illustrated in src/Fantomas.Core.Tests/TriviaAssignmentTests.fs.
val enrichTree: config: FormatConfig -> sourceText: ISourceText -> ast: ParsedInput -> tree: Oak -> Oak * RecordedTrivia

/// Record the editor's cursor in the Oak so that CodePrinter can report where it ends up.
/// When the cursor sits inside a `SingleTextNode`, the node remembers it and the printer reports
/// the same offset into the printed text. Otherwise a `Cursor` trivia is attached to the smallest
/// node around it, and the printer reports the position after that node. Mutates `tree` in place.
/// The end-to-end behaviour is in src/Fantomas.Core.Tests/CursorTests.fs.
val insertCursor: tree: Oak -> cursor: pos -> Oak
