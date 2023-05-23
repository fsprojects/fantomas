module Fantomas.Core.ImmutableArray

open System
open System.Collections.Generic
open System.Collections.Immutable

type ImmutableArrayBuilderCode<'T> = delegate of byref<ImmutableArray<'T>.Builder> -> unit

type ImmutableArrayViaBuilder<'T>(builder: ImmutableArray<'T>.Builder) =
    member val Builder: ImmutableArray<'T>.Builder = builder with get

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
        builder.ToImmutableArray()

let immarray<'T> = ImmutableArrayViaBuilder<'T>(ImmutableArray.CreateBuilder<'T>())

let fixedImmarray<'T> capacity =
    ImmutableArrayViaBuilder(ImmutableArray.CreateBuilder<'T>(initialCapacity = capacity))

type immarray<'T> = ImmutableArray<'T>

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
