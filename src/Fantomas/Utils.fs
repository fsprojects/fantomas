namespace Fantomas

open System
open System.Text.RegularExpressions

[<RequireQualifiedAccess>]
module Char =
    let escape c =
        match c with
        | '\r' -> @"\r"
        | '\n' -> @"\n"
        | '\t' -> @"\t"
        | '\\' -> @"\\"
        | '\b' -> @"\b"
        | '\f' -> @"\f"
        | _ -> c.ToString()

[<RequireQualifiedAccess>]
module String =
    let normalizeNewLine (str: string) =
        str.Replace("\r\n", "\n").Replace("\r", "\n")

    let normalizeThenSplitNewLine (str: string) = (normalizeNewLine str).Split('\n')

    let startsWithOrdinal (prefix: string) (str: string) =
        str.StartsWith(prefix, StringComparison.Ordinal)

    let private lengthWithoutSpaces (str: string) = str.Replace(" ", String.Empty).Length

    let private hashRegex = @"^\s*#(if|elseif|else|endif).*"

    let private splitWhenHash (newline: string) (source: string) : string list =
        let lines =
            source.Split([| newline |], options = StringSplitOptions.None)

        let hashLineIndexes =
            lines
            |> Array.mapi (fun idx line -> Regex.IsMatch(line, hashRegex), idx)
            |> Array.choose (fun (isMatch, idx) -> if isMatch then Some idx else None)
            |> Array.toList

        let hashLineIndexesWithStart =
            match List.tryHead hashLineIndexes with
            | Some 0 -> hashLineIndexes
            | _ -> 0 :: hashLineIndexes

        let rec loop (indexes: int list) (finalContinuation: string [] list -> string [] list) =
            match indexes with
            | [] -> finalContinuation []
            | i1 :: i2 :: rest ->
                let chunk = lines.[i1..(i2 - 1)]
                chunk.[0] <- chunk.[0].TrimStart()
                loop (i2 :: rest) (fun otherChunks -> chunk :: otherChunks |> finalContinuation)
            | [ lastIndex ] ->
                let chunk = lines.[lastIndex..]
                chunk.[0] <- chunk.[0].TrimStart()
                finalContinuation [ chunk ]

        loop hashLineIndexesWithStart id
        |> List.map (String.concat newline)

    let merge (newline: string) (a: string) (b: string) : string =
        let aChunks = splitWhenHash newline a
        let bChunks = splitWhenHash newline b

        if List.length aChunks <> List.length bChunks then
            Dbg.print (aChunks, bChunks)

            failwithf
                """Fantomas is trying to format the input multiple times due to the detect of multiple defines.
There is a problem with merging all the code back togheter. Please raise an issue at https://github.com/fsprojects/fantomas/issues."""

        List.zip aChunks bChunks
        |> List.map
            (fun (a', b') ->
                let la = lengthWithoutSpaces a'
                let lb = lengthWithoutSpaces b'

                if la <> lb then
                    if la > lb then a' else b'
                else if String.length a' < String.length b' then
                    a'
                else
                    b')

        |> String.concat newline

    let empty = String.Empty

    let isNotNullOrEmpty = String.IsNullOrEmpty >> not
    let isNotNullOrWhitespace = String.IsNullOrWhiteSpace >> not

    let isMultiline s =
        normalizeNewLine s |> String.exists ((=) '\n')

module Cache =
    let alreadyVisited<'key when 'key: not struct> () =
        let cache =
            System.Collections.Generic.HashSet<'key>([], HashIdentity.Reference)

        fun key ->
            if cache.Contains key then
                true
            else
                cache.Add key |> ignore
                false

    let memoizeBy (g: 'a -> 'c) (f: 'a -> 'b) =
        let cache =
            System.Collections.Concurrent.ConcurrentDictionary<_, _>(HashIdentity.Structural)

        fun x -> cache.GetOrAdd(Some(g x), lazy (f x)).Force()

    [<CustomEquality; NoComparison>]
    type LambdaEqByRef<'a, 'b> =
        | LambdaEqByRef of ('a -> 'b)
        override this.Equals(obj) =
            match obj with
            | :? LambdaEqByRef<'a, 'b> as y ->
                let (LambdaEqByRef f) = this
                let (LambdaEqByRef g) = y
                Object.ReferenceEquals(f, g)
            | _ -> false

        override this.GetHashCode() = 0

module Dict =
    let tryGet k (d: System.Collections.Generic.IDictionary<_, _>) =
        let r, x = d.TryGetValue k
        if r then Some x else None

module List =
    let appendItem l i = l @ [ i ]

    let prependItem l i = i :: l

    let takeWhileState f state l =
        let mutable s = state

        l
        |> List.takeWhile
            (fun x ->
                let s', r = f s x
                s <- s'
                r)

    let isNotEmpty l = (List.isEmpty >> not) l

    let moreThanOne =
        function
        | []
        | [ _ ] -> false
        | _ -> true

    let splitAround<'a> (n: int) (xs: 'a list) : ('a list * ('a * 'a list) option) option =
        let rec go n heads rest =
            if n = 0 then
                match rest with
                | [] -> Some(List.rev heads, None)
                | x :: rest -> Some(List.rev heads, Some(x, rest))
            else
                match rest with
                | [] -> None
                | x :: rest -> go (n - 1) (x :: heads) rest

        if n < 0 then None else go n [] xs


module Map =
    let tryFindOrDefault (defaultValue: 'g) (key: 't) (map: Map<'t, 'g>) =
        match Map.tryFind key map with
        | Some v -> v
        | None -> defaultValue

    let tryFindOrEmptyList (key: 't) (map: Map<'t, 'g list>) = tryFindOrDefault [] key map

module Async =
    let map f computation =
        async.Bind(computation, f >> async.Return)

[<RequireQualifiedAccess>]
module Continuation =
    let rec sequence<'a, 'ret> (recursions: (('a -> 'ret) -> 'ret) list) (finalContinuation: 'a list -> 'ret) : 'ret =
        match recursions with
        | [] -> [] |> finalContinuation
        | recurse :: recurses -> recurse (fun ret -> sequence recurses (fun rets -> ret :: rets |> finalContinuation))
