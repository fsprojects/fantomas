namespace Fantomas.Core

open System
open Microsoft.FSharp.Core.CompilerServices

[<RequireQualifiedAccess>]
module String =

    let startsWithOrdinal (prefix: string) (str: string) : bool =
        str.StartsWith(prefix, StringComparison.Ordinal)

    let endsWithOrdinal (postfix: string) (str: string) : bool =
        str.EndsWith(postfix, StringComparison.Ordinal)

    let empty: string = String.Empty
    let isNotNullOrEmpty: string -> bool = String.IsNullOrEmpty >> not
    let isNotNullOrWhitespace: string -> bool = String.IsNullOrWhiteSpace >> not

module List =
    let chooseState<'a, 'b, 'c> (f: 'a -> 'b -> 'a * 'c option) (state: 'a) (l: 'b list) : 'c list =
        let mutable s = state

        l
        |> List.choose (fun x ->
            let s', r = f s x
            s <- s'
            r)

    let isNotEmpty<'a> (l: 'a list) : bool = (List.isEmpty >> not) l

    let moreThanOne<'a> : 'a list -> bool =
        function
        | []
        | [ _ ] -> false
        | _ -> true

    let mapWithLast<'a, 'b> (f: 'a -> 'b) (g: 'a -> 'b) (xs: 'a list) : 'b list =
        let rec visit xs continuation =
            match xs with
            | [] -> continuation []
            | [ last ] -> continuation [ g last ]
            | head :: tail -> visit tail (fun ys -> f head :: ys |> continuation)

        visit xs id

    let cutOffLast<'a> (list: 'a list) : 'a list =
        let mutable headList = ListCollector<'a>()

        let rec visit list =
            match list with
            | []
            | [ _ ] -> ()
            | head :: tail ->
                headList.Add(head)
                visit tail

        visit list
        headList.Close()

    let foldWithLast<'state, 'item>
        (f: 'state -> 'item -> 'state)
        (g: 'state -> 'item -> 'state)
        (initialState: 'state)
        (items: 'item list)
        : 'state =
        let rec visit acc xs =
            match xs with
            | [] -> acc
            | [ last ] -> g acc last
            | head :: tail -> visit (f acc head) tail

        visit initialState items

module Async =
    let map<'a, 'b> (f: 'a -> 'b) (computation: Async<'a>) : Async<'b> =
        async.Bind(computation, f >> async.Return)

[<RequireQualifiedAccess>]
module Continuation =
    let rec sequence<'a, 'ret> (recursions: (('a -> 'ret) -> 'ret) list) (finalContinuation: 'a list -> 'ret) : 'ret =
        match recursions with
        | [] -> [] |> finalContinuation
        | recurse :: recurses -> recurse (fun ret -> sequence recurses (fun rets -> ret :: rets |> finalContinuation))
