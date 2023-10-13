namespace Fantomas.Core

open System
open Microsoft.FSharp.Core.CompilerServices

[<RequireQualifiedAccess>]
module String =
    let startsWithOrdinal (prefix: string) (str: string) =
        str.StartsWith(prefix, StringComparison.Ordinal)

    let endsWithOrdinal (postfix: string) (str: string) =
        str.EndsWith(postfix, StringComparison.Ordinal)

    let empty = String.Empty
    let isNotNullOrEmpty = String.IsNullOrEmpty >> not
    let isNotNullOrWhitespace = String.IsNullOrWhiteSpace >> not

module List =
    let chooseState f state l =
        let mutable s = state

        l
        |> List.choose (fun x ->
            let s', r = f s x
            s <- s'
            r)

    let isNotEmpty l = (List.isEmpty >> not) l

    let moreThanOne =
        function
        | []
        | [ _ ] -> false
        | _ -> true

    let partitionWhile (f: int -> 'a -> bool) (xs: 'a list) : 'a list * 'a list =
        let rec go i before after =
            match after with
            | head :: tail ->
                match f i head with
                | true -> go (i + 1) (head :: before) tail
                | false -> List.rev before, after
            | [] -> List.rev before, after

        go 0 [] xs

    let mapWithLast (f: 'a -> 'b) (g: 'a -> 'b) (xs: 'a list) =
        let rec visit xs continuation =
            match xs with
            | [] -> continuation []
            | [ last ] -> continuation [ g last ]
            | head :: tail -> visit tail (fun ys -> f head :: ys |> continuation)

        visit xs id

    let cutOffLast list =
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

    let foldWithLast
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
    let map f computation =
        async.Bind(computation, f >> async.Return)

[<RequireQualifiedAccess>]
module Continuation =
    let rec sequence<'a, 'ret> (recursions: (('a -> 'ret) -> 'ret) list) (finalContinuation: 'a list -> 'ret) : 'ret =
        match recursions with
        | [] -> [] |> finalContinuation
        | recurse :: recurses -> recurse (fun ret -> sequence recurses (fun rets -> ret :: rets |> finalContinuation))
