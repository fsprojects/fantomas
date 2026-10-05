namespace Fantomas.Core

open Fantomas.FCS.Text

[<RequireQualifiedAccess>]
module RangeHelpers =
    /// Checks if Range B is fully contained by Range A
    val rangeContainsRange: a: Range -> b: Range -> bool
    val rangeEq: (range -> range -> bool)
    /// Whether `r2` starts exactly where `r1` ends, on the same line and in the same file.
    /// Nothing, not even a space, sits between them.
    val isAdjacentTo: r1: Range -> r2: Range -> bool
    /// Range.range0 starts at line 1, column 0
    /// This range starts at line 0, column 0
    val absoluteZeroRange: Range
    /// Whether a range is `absoluteZeroRange`, the range of the module of a file without code. A
    /// source has no line 0, so the start line says it. Reading `absoluteZeroRange` instead
    /// initializes it, which enters the compiler's `range.fs` through its `Range` module. Doing that
    /// while another thread enters the same file through its first parse can leave that parse with
    /// a null file index, as `ParserWarmupFixture` in `Fantomas.Core.Tests` explains.
    val isAbsoluteZero: r: Range -> bool

/// Split the range of a node into the ranges of the tokens that delimit it, so the transformer can
/// give an opening or closing token a `SingleTextNode` of its own. `size` is the length of the
/// token in characters: 1 for `(`, 2 for `[|` or `{|`. The ranges are computed from the ends of the
/// node's range, not looked up, so they are only right when the token really is the first or last
/// thing in the node.
module RangePatterns =
    /// The opening token, the whole range, and the closing token, both tokens `size` long.
    val (|StartEndRange|): size: int -> range: range -> range * range * range
    /// The opening token, `size` long, and the whole range.
    val (|StartRange|): size: int -> range: range -> range * range
    /// The closing token, `size` long, and the whole range.
    val (|EndRange|): size: int -> range: range -> range * range
