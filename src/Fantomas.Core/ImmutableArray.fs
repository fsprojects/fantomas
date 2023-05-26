module Fantomas.Core.ImmutableArray

open System
open System.Collections.Immutable
open Microsoft.FSharp.Core

type immarray<'T> = ImmutableArray<'T>

type ImmutableArrayViaBuilder<'T>(builder: ImmutableArray<'T>.Builder) =

    member this.Run _ =
        builder.Capacity <- builder.Count
        builder.MoveToImmutable()

    member this.Yield(item: 'T) = builder.Add(item)

    member this.YieldFrom(items: 'T seq) = builder.AddRange(items)

    member this.Combine(_, r) = r

    member this.Delay f = f ()

    member this.For(items, f) =
        for i in items do
            f i

    member this.Zero() = ()

let immarray<'T> = ImmutableArrayViaBuilder(ImmutableArray.CreateBuilder<'T>())

type ImmutableArray<'T> with

    member this.IsNotEmpty: bool = not this.IsEmpty

let (|EmptyImmutableArray|SingleItemImmutableArray|HeadAndTailInImmutableArray|) (array: 'T immarray) =
    if array.IsEmpty then
        EmptyImmutableArray
    elif array.Length = 1 then
        SingleItemImmutableArray(array.[0])
    else
        let rest = array.Slice(1, array.Length - 1)
        HeadAndTailInImmutableArray(array.[0], rest)

let (|TwoItemsImmutableArray|_|) (array: 'T immarray) =
    if array.Length <> 2 then
        None
    else
        Some(array.[0], array.[1])

[<RequireQualifiedAccess>]
module ImmutableArray =
    let empty<'T> = ImmutableArray<'T>.Empty
    let singleton<'T> (item: 'T) = ImmutableArray.Create(item)
    let ofSeq (xs: 'T seq) = ImmutableArray.CreateRange(xs)

    let map (mapper: 'T -> 'U) (arr: 'T immarray) : 'U immarray =
        match arr.Length with
        | 0 -> ImmutableArray.Empty
        | 1 -> ImmutableArray.Create(mapper arr.[0])
        | _ ->
            let builder = ImmutableArray.CreateBuilder(arr.Length)

            for i = 0 to arr.Length - 1 do
                builder.Add(mapper arr.[i])

            builder.MoveToImmutable()

    let mapi (mapper: int -> 'T -> 'U) (arr: 'T immarray) : 'U immarray =
        match arr.Length with
        | 0 -> ImmutableArray.Empty
        | 1 -> ImmutableArray.Create(mapper 0 arr.[0])
        | _ ->
            let builder = ImmutableArray.CreateBuilder(arr.Length)

            for i = 0 to arr.Length - 1 do
                builder.Add(mapper i arr.[i])

            builder.MoveToImmutable()

    let mapList (mapper: 'T -> 'U) (list: 'T list) : 'U immarray =
        let builder: ImmutableArray<'U>.Builder =
            ImmutableArray.CreateBuilder<'U>(initialCapacity = list.Length)

        let rec visit xs =
            match xs with
            | [] -> ()
            | head :: rest ->
                builder.Add(mapper head)
                visit rest

        visit list
        builder.MoveToImmutable()

    let tryPick chooser (immutableArray: 'T immarray) =
        let rec loop i =
            if i >= immutableArray.Length then
                None
            else
                match chooser immutableArray.[i] with
                | None -> loop (i + 1)
                | res -> res

        loop 0

    let last (immutableArray: 'T immarray) : 'T =
        assert (not immutableArray.IsEmpty)
        immutableArray.[immutableArray.Length - 1]

    let tryLast (immutableArray: 'T immarray) : 'T option =
        if immutableArray.IsEmpty then
            None
        else
            Some immutableArray.[immutableArray.Length - 1]

    let filter predicate (immutableArray: 'T immarray) : 'T immarray =
        if immutableArray.IsEmpty then
            immutableArray
        else
            let builder = ImmutableArray.CreateBuilder(immutableArray.Length)

            for i = 0 to immutableArray.Length - 1 do
                if predicate immutableArray.[i] then
                    builder.Add(immutableArray.[i])

            builder.Capacity <- builder.Count
            builder.MoveToImmutable()

    let collect (collector: 'T -> 'U immarray) (arrays: 'T immarray) : 'U immarray =
        match arrays.Length with
        | 0 -> ImmutableArray.Empty
        | 1 -> collector arrays.[0]
        | _ ->
            let builder = ImmutableArray.CreateBuilder<'U>()

            for i = 0 to arrays.Length - 1 do
                builder.AddRange(collector arrays.[i])

            builder.Capacity <- builder.Count
            builder.MoveToImmutable()

    let tryHead (array: 'T immarray) : 'T option =
        if array.IsEmpty then None else Some array.[0]

    let fold folder state (arr: 'T immarray) =
        let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(folder)
        let mutable state = state

        for i = 0 to arr.Length - 1 do
            state <- f.Invoke(state, arr.[i])

        state

    let exists predicate (array: 'T immarray) =
        let len = array.Length

        let rec loop i =
            i < len && (predicate array.[i] || loop (i + 1))

        len > 0 && loop 0

    let forall predicate (arr: 'T immarray) =
        let len = arr.Length

        let rec loop i =
            i >= len || (predicate arr.[i] && loop (i + 1))

        loop 0

    let chunkBySize (chunkSize: int) (array: 'T immarray) =
        let startIndexes = [| 0..chunkSize .. array.Length |]

        let builder =
            ImmutableArray.CreateBuilder<'T immarray>(initialCapacity = startIndexes.Length)

        for startIndex in startIndexes do
            let sliceSize = Math.Min(array.Length - startIndex, chunkSize)
            builder.Add(array.Slice(startIndex, sliceSize))

        builder.MoveToImmutable()

    let choose (chooser: 'T -> 'U option) (array: 'T immarray) =
        if array.IsEmpty then
            empty
        else
            let builder = ImmutableArray.CreateBuilder<'U>(initialCapacity = array.Length)

            for item in array do
                match chooser item with
                | None -> ()
                | Some u -> builder.Add u

            builder.Capacity <- builder.Count
            builder.MoveToImmutable()
