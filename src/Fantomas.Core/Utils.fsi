namespace Fantomas.Core

[<RequireQualifiedAccess>]
module String =
    val startsWithOrdinal: prefix: string -> str: string -> bool
    val empty: string
    val isNotNullOrEmpty: (string -> bool)
    val isNotNullOrWhitespace: (string -> bool)

module List =
    val chooseState: f: ('a -> 'b -> 'a * 'c option) -> state: 'a -> l: 'b list -> 'c list
    val isNotEmpty: l: 'a list -> bool
    val moreThanOne: ('a list -> bool)
    val partitionWhile: f: (int -> 'a -> bool) -> xs: 'a list -> 'a list * 'a list
    val mapWithLast: f: ('a -> 'b) -> g: ('a -> 'b) -> xs: 'a list -> 'b list
    /// Removes the last element of a list
    val cutOffLast: 'a list -> 'a list

module Async =
    val map: f: ('a -> 'b) -> computation: Async<'a> -> Async<'b>

[<RequireQualifiedAccess>]
module Continuation =
    val sequence<'a, 'ret> : recursions: (('a -> 'ret) -> 'ret) list -> finalContinuation: ('a list -> 'ret) -> 'ret
