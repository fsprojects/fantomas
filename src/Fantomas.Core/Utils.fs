namespace Fantomas.Core

open System
open System.Text.RegularExpressions
open Microsoft.FSharp.Core.CompilerServices

[<RequireQualifiedAccess>]
module String =
    let startsWithOrdinal (prefix: string) (str: string) =
        str.StartsWith(prefix, StringComparison.Ordinal)

    let lengthWithoutSpaces (str: string) = str.Replace(" ", String.Empty).Length

    let hashRegex = @"^\s*#(if|elseif|else|endif).*"

    let private splitWhenHash (newline: string) (source: string) : string list =
        let lines = source.Split([| newline |], options = StringSplitOptions.None)

        let hashLineIndexes =
            lines
            |> Array.mapi (fun idx line -> Regex.IsMatch(line, hashRegex), idx)
            |> Array.choose (fun (isMatch, idx) -> if isMatch then Some idx else None)
            |> Array.toList

        let hashLineIndexesWithStart =
            match List.tryHead hashLineIndexes with
            | Some 0 -> hashLineIndexes
            | _ -> 0 :: hashLineIndexes

        let rec loop (indexes: int list) (finalContinuation: string[] list -> string[] list) =
            match indexes with
            | [] -> finalContinuation []
            | i1 :: i2 :: rest ->
                let chunk = lines.[i1 .. (i2 - 1)]
                chunk.[0] <- chunk.[0].TrimStart()
                loop (i2 :: rest) (fun otherChunks -> chunk :: otherChunks |> finalContinuation)
            | [ lastIndex ] ->
                let chunk = lines.[lastIndex..]
                chunk.[0] <- chunk.[0].TrimStart()
                finalContinuation [ chunk ]

        loop hashLineIndexesWithStart id |> List.map (String.concat newline)

    let splitInFragments (newline: string) (items: (string list * string) list) : (string list * string list) list =
        List.map
            (fun (defines, code) ->
                let fragments = splitWhenHash newline code
                defines, fragments)
            items

    let merge (aChunks: string list) (bChunks: string list) : string list =
        List.zip aChunks bChunks
        |> List.map (fun (a', b') ->
            let la = lengthWithoutSpaces a'
            let lb = lengthWithoutSpaces b'

            if la <> lb then
                if la > lb then a' else b'
            else if String.length a' < String.length b' then
                a'
            else
                b')

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

module Async =
    let map f computation =
        async.Bind(computation, f >> async.Return)

[<RequireQualifiedAccess>]
module Continuation =
    let rec sequence<'a, 'ret> (recursions: (('a -> 'ret) -> 'ret) list) (finalContinuation: 'a list -> 'ret) : 'ret =
        match recursions with
        | [] -> [] |> finalContinuation
        | recurse :: recurses -> recurse (fun ret -> sequence recurses (fun rets -> ret :: rets |> finalContinuation))
