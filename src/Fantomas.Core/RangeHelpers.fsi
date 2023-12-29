namespace Fantomas.Core

open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
module RangeHelpers =
    /// Checks if Range B is fully contained by Range A
    val rangeContainsRange: a: range -> b: range -> bool
    val rangeEq: (range -> range -> bool)
    val isAdjacentTo: r1: range -> r2: range -> bool

module RangePatterns =
    val (|StartEndRange|): size: int -> range: range -> range * range * range
    val (|StartRange|): size: int -> range: range -> range * range
    val (|EndRange|): size: int -> range: range -> range * range
