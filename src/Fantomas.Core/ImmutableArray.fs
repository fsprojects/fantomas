module Fantomas.Core.ImmutableArray

open System
open System.Collections.Generic
open System.Collections.Immutable

type ImmutableArrayBuilderCode<'T> = delegate of byref<ImmutableArray<'T>.Builder> -> unit

type ImmutableArrayViaBuilder<'T>(builder: ImmutableArray<'T>.Builder) =
    member val public Builder: ImmutableArray<'T>.Builder = builder with get

    member inline _.Delay([<InlineIfLambda>] f: unit -> ImmutableArrayBuilderCode<'T>) : ImmutableArrayBuilderCode<'T> =
        ImmutableArrayBuilderCode<_>(fun sm -> (f ()).Invoke &sm)

    member inline _.Zero() : ImmutableArrayBuilderCode<'T> =
        ImmutableArrayBuilderCode<_>(fun _sm -> ())

    member inline _.Combine
        (
            [<InlineIfLambda>] part1: ImmutableArrayBuilderCode<'T>,
            [<InlineIfLambda>] part2: ImmutableArrayBuilderCode<'T>
        ) : ImmutableArrayBuilderCode<'T> =
        ImmutableArrayBuilderCode<_>(fun sm ->
            part1.Invoke &sm
            part2.Invoke &sm)

    member inline _.While
        (
            [<InlineIfLambda>] condition: unit -> bool,
            [<InlineIfLambda>] body: ImmutableArrayBuilderCode<'T>
        ) : ImmutableArrayBuilderCode<'T> =
        ImmutableArrayBuilderCode<_>(fun sm ->
            while condition () do
                body.Invoke &sm)

    member inline _.TryWith
        (
            [<InlineIfLambda>] body: ImmutableArrayBuilderCode<'T>,
            [<InlineIfLambda>] handler: exn -> ImmutableArrayBuilderCode<'T>
        ) : ImmutableArrayBuilderCode<'T> =
        ImmutableArrayBuilderCode<_>(fun sm ->
            try
                body.Invoke &sm
            with exn ->
                (handler exn).Invoke &sm)

    member inline _.TryFinally
        (
            [<InlineIfLambda>] body: ImmutableArrayBuilderCode<'T>,
            compensation: unit -> unit
        ) : ImmutableArrayBuilderCode<'T> =
        ImmutableArrayBuilderCode<_>(fun sm ->
            try
                body.Invoke &sm
            with _ ->
                compensation ()
                reraise ()

            compensation ())

    member inline b.Using
        (
            disp: #IDisposable,
            [<InlineIfLambda>] body: #IDisposable -> ImmutableArrayBuilderCode<'T>
        ) : ImmutableArrayBuilderCode<'T> =
        // A using statement is just a try/finally with the finally block disposing if non-null.
        b.TryFinally(
            (fun sm -> (body disp).Invoke &sm),
            (fun () ->
                if not (isNull (box disp)) then
                    disp.Dispose())
        )

    member inline b.For
        (
            sequence: seq<'TElement>,
            [<InlineIfLambda>] body: 'TElement -> ImmutableArrayBuilderCode<'T>
        ) : ImmutableArrayBuilderCode<'T> =
        b.Using(
            sequence.GetEnumerator(),
            (fun e -> b.While((fun () -> e.MoveNext()), (fun sm -> (body e.Current).Invoke &sm)))
        )

    member inline _.Yield(v: 'T) : ImmutableArrayBuilderCode<'T> =
        ImmutableArrayBuilderCode<_>(fun sm -> sm.Add v)

    member inline b.YieldFrom(source: IEnumerable<'T>) : ImmutableArrayBuilderCode<'T> =
        ImmutableArrayBuilderCode<_>(fun sm -> sm.AddRange source)

    member inline b.Run([<InlineIfLambda>] code: ImmutableArrayBuilderCode<'T>) : ImmutableArray<'T> =
        let mutable builder = b.Builder
        code.Invoke &builder
        builder.MoveToImmutable()

let immarray<'T> capacity =
    ImmutableArrayViaBuilder(ImmutableArray.CreateBuilder<'T>(initialCapacity = capacity))

type immarray<'T> = ImmutableArray<'T>

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
        builder.ToImmutable()

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

            builder.ToImmutable()

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
