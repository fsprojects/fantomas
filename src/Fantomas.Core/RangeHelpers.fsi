namespace Fantomas.Core

open Fantomas.FCS.Text

[<RequireQualifiedAccess>]
module RangeHelpers =
    /// Checks if Range B is fully contained by Range A
    val rangeContainsRange: a: Range -> b: Range -> bool
    val rangeEq: (range -> range -> bool)
    val isAdjacentTo: r1: Range -> r2: Range -> bool

module RangePatterns =
    val (|StartEndRange|): size: int -> range: range -> range * range * range
    val (|StartRange|): size: int -> range: range -> range * range
    val (|EndRange|): size: int -> range: range -> range * range
