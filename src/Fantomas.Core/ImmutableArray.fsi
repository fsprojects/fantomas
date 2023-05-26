module Fantomas.Core.ImmutableArray

open System.Collections.Immutable

type immarray<'T> = ImmutableArray<'T>

type ImmutableArrayViaBuilder<'T> =
    new: builder: ImmutableArray<'T>.Builder -> ImmutableArrayViaBuilder<'T>
    member Run: 'a -> 'T immarray
    member Yield: 'T -> unit
    member YieldFrom: 'T seq -> unit
    member Combine: 'a * 'b -> 'b
    member Delay: (unit -> 'a) -> 'a
    member For: items: 'a seq * f: ('a -> unit) -> unit
    member Zero: unit -> unit

val immarray<'T> : capacity: int -> ImmutableArrayViaBuilder<'T>

type ImmutableArray<'T> with

    member IsNotEmpty: bool

val (|EmptyImmutableArray|SingleItemImmutableArray|HeadAndTailInImmutableArray|):
    'T immarray -> Choice<unit, 'T, 'T * 'T immarray>

val (|TwoItemsImmutableArray|_|): 'T immarray -> ('T * 'T) option

[<RequireQualifiedAccess>]
module ImmutableArray =
    val empty<'T> : 'T immarray
    val singleton: item: 'T -> 'T immarray
    val ofSeq: xs: 'T seq -> 'T immarray
    val map: mapper: ('T -> 'U) -> 'T immarray -> 'U immarray
    val mapi: mapper: (int -> 'T -> 'U) -> 'T immarray -> 'U immarray
    /// Special mapping function to map FSC lists to immutable arrays
    val mapList: mapper: ('T -> 'U) -> 'T list -> immarray<'U>
    val tryPick: chooser: ('T -> 'U option) -> immutableArray: 'T immarray -> 'U option
    val last: immarray<'T> -> 'T
    val tryLast: immarray<'T> -> 'T option
    val filter: predicate: ('T -> bool) -> immarray<'T> -> immarray<'T>
    val collect: collector: ('T -> 'U immarray) -> arrays: 'T immarray -> 'U immarray
    val tryHead: array: 'T immarray -> 'T option
    val fold: folder: ('State -> 'T -> 'State) -> 'State -> 'T immarray -> 'State
    val exists: predicate: ('T -> bool) -> 'T immarray -> bool
    val forall: ('T -> bool) -> immarray<'T> -> bool
    val chunkBySize: chunkSize: int -> 'T immarray -> 'T immarray immarray
    val choose: chooser: ('T -> 'U option) -> immarray<'T> -> ImmutableArray<'U>
