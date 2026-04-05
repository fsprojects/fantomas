namespace Fantomas.Core

[<RequireQualifiedAccess>]
module String =
    val startsWithOrdinal: prefix: string -> str: string -> bool
    val endsWithOrdinal: postfix: string -> str: string -> bool
    val empty: string
    val isNotNullOrEmpty: (string -> bool)
    val isNotNullOrWhitespace: (string -> bool)

module List =
    /// Like List.choose but threads a state value through the computation.
    /// The function f receives the current state and element, and returns the next state paired with an optional result.
    val chooseState: f: ('a -> 'b -> 'a * 'c option) -> state: 'a -> l: 'b list -> 'c list
    val isNotEmpty: l: 'a list -> bool
    val moreThanOne: ('a list -> bool)

    /// Returns the elements of xs split at the first index where f returns false.
    /// The index (starting at 0) is passed to f along with each element.
    val partitionWhile: f: (int -> 'a -> bool) -> xs: 'a list -> 'a list * 'a list

    /// Maps f over all elements except the last, and g over the last element.
    /// Returns an empty list when xs is empty.
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
