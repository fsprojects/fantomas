module Fantomas.Core.ImmutableArray

open System
open System.Collections.Generic
open System.Collections.Immutable

type ImmutableArrayBuilderCode<'T> = delegate of byref<ImmutableArray<'T>.Builder> -> unit

type ImmutableArrayViaBuilder<'T> =
    new: builder: ImmutableArray<'T>.Builder -> ImmutableArrayViaBuilder<'T>
    member public Builder: ImmutableArray<'T>.Builder with get
    member inline Delay: (unit -> ImmutableArrayBuilderCode<'T>) -> ImmutableArrayBuilderCode<'T>
    member inline Zero: unit -> ImmutableArrayBuilderCode<'T>

    member inline Combine:
        ImmutableArrayBuilderCode<'T> * ImmutableArrayBuilderCode<'T> -> ImmutableArrayBuilderCode<'T>

    member inline While: (unit -> bool) * ImmutableArrayBuilderCode<'T> -> ImmutableArrayBuilderCode<'T>

    member inline TryWith:
        ImmutableArrayBuilderCode<'T> * (exn -> ImmutableArrayBuilderCode<'T>) -> ImmutableArrayBuilderCode<'T>

    member inline TryFinally: ImmutableArrayBuilderCode<'T> * (unit -> unit) -> ImmutableArrayBuilderCode<'T>

    member inline Using:
        'a * ('a -> ImmutableArrayBuilderCode<'T>) -> ImmutableArrayBuilderCode<'T> when 'a :> IDisposable

    member inline For: seq<'TElement> * ('TElement -> ImmutableArrayBuilderCode<'T>) -> ImmutableArrayBuilderCode<'T>
    member inline Yield: 'T -> ImmutableArrayBuilderCode<'T>
    member inline YieldFrom: IEnumerable<'T> -> ImmutableArrayBuilderCode<'T>
    member inline Run: ImmutableArrayBuilderCode<'T> -> ImmutableArray<'T>

val immarray<'T> : capacity: int -> ImmutableArrayViaBuilder<'T>

type immarray<'T> = ImmutableArray<'T>

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
