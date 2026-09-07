module internal Fantomas.Core.Selection

open Fantomas.FCS.Text

/// Format one node of the source and return its formatted text with the range it replaces.
///
/// The selection is first trimmed to the code inside it: leading and trailing whitespace lines are
/// dropped and the columns moved to the first and last non-blank character. The trimmed range has
/// to match a node exactly; it is that trimmed range which is returned, not the one passed in.
///
/// The node is then formatted on its own. Most nodes are wrapped in a synthetic module. Nodes that
/// cannot stand alone, a type or a pattern, are placed in a synthetic binding, and the formatted
/// node is cut back out of the result by parsing it again. The page width is reduced by the start
/// column so the result fits where it came from, and the final newline is left off.
///
/// Raises `FormatException` when the source does not parse, when no node matches the trimmed
/// selection, when the node kind is not supported, or when the node cannot be found again in the
/// formatted synthetic tree. Illustrated in src/Fantomas.Core.Tests/FormattingSelectionOnlyTests.fs.
val formatSelection:
    config: FormatConfig -> isSignature: bool -> selection: range -> sourceText: ISourceText -> Async<string * range>
