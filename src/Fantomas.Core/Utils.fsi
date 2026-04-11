namespace Fantomas.Core

[<RequireQualifiedAccess>]
module String =
    val startsWithOrdinal: prefix: string -> str: string -> bool
    val endsWithOrdinal: postfix: string -> str: string -> bool
    val empty: string
    val isNotNullOrEmpty: (string -> bool)
    val isNotNullOrWhitespace: (string -> bool)
    /// Returns the visual column width of a string, counting Unicode grapheme clusters.
    /// Unlike String.length, this correctly handles combining characters (e.g. diacritics)
    /// which attach to a preceding character and do not advance the visual column.
    /// Uses a fast path for pure-ASCII strings (no allocation).
    val visualWidth: s: string -> int

module List =
    val chooseState: f: ('a -> 'b -> 'a * 'c option) -> state: 'a -> l: 'b list -> 'c list
    val isNotEmpty: l: 'a list -> bool
    val moreThanOne: ('a list -> bool)
    val partitionWhile: f: (int -> 'a -> bool) -> xs: 'a list -> 'a list * 'a list
    val mapWithLast: f: ('a -> 'b) -> g: ('a -> 'b) -> xs: 'a list -> 'b list
    /// Removes the last element of a list
    val cutOffLast: 'a list -> 'a list

    /// Similar to a List.fold but pass in another fold function for when the last item is reached.
    val foldWithLast:
        f: ('state -> 'item -> 'state) ->
        g: ('state -> 'item -> 'state) ->
        initialState: 'state ->
        items: 'item list ->
            'state

module Async =
    val map: f: ('a -> 'b) -> computation: Async<'a> -> Async<'b>

[<RequireQualifiedAccess>]
module Continuation =
    val sequence<'a, 'ret> : recursions: (('a -> 'ret) -> 'ret) list -> finalContinuation: ('a list -> 'ret) -> 'ret
