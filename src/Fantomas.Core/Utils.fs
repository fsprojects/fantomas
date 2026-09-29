namespace Fantomas.Core

open System
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Reflection

[<RequireQualifiedAccess>]
module UnionCase =

    let name (value: 'T) : string =
        match box value with
        | null -> typeof<'T>.Name
        | boxed ->

        let runtimeType: Type = boxed.GetType()

        // A case with fields is a class of its own, nested in the union and named after the case,
        // so naming it needs nothing a Native AOT build may have trimmed away. So is a case without
        // fields of a union with fewer than four cases, which the compiler tells apart by type
        // rather than by tag, named with a leading underscore. A case name has to start with an
        // uppercase letter, so an underscore there is always that one.
        match runtimeType.DeclaringType with
        | declaringType when not (isNull declaringType) && runtimeType.BaseType = declaringType ->
            let caseName: string = runtimeType.Name.TrimStart '_'
            $"%s{declaringType.Name}.%s{caseName}"
        | _ ->

        // A case without fields is an instance of the union itself, and only F# reflection can tell
        // which one. Under Native AOT that works or not depending on what the trimmer kept, and
        // where it does not the union is named without the case. The name goes into the message of
        // an exception that is already being raised, which a failure here must not replace.
        try
            if not (FSharpType.IsUnion(runtimeType, true)) then
                runtimeType.Name
            else

            let case, _ = FSharpValue.GetUnionFields(boxed, runtimeType, true)
            $"%s{runtimeType.Name}.%s{case.Name}"
        with _ ->
            runtimeType.Name

[<RequireQualifiedAccess>]
module Triage =

    // A try rather than a check of `RuntimeFeature.IsDynamicCodeSupported`, which netstandard2.0
    // does not have. Every failure is caught, not only the NotSupportedException of Native AOT: the
    // dump rides along on an exception that is already being raised, and failing to write it must
    // not replace that exception with its own.
    let dump (value: 'T) : string =
        try
            // fsharpanalyzer: ignore-line-next FANTOMAS-PRINTF-001
            $"%A{value}"
        with _ ->
            match box value with
            | null -> typeof<'T>.FullName
            | boxed -> boxed.GetType().FullName

[<RequireQualifiedAccess>]
module String =

    let startsWithOrdinal (prefix: string) (str: string) =
        str.StartsWith(prefix, StringComparison.Ordinal)

    let endsWithOrdinal (postfix: string) (str: string) =
        str.EndsWith(postfix, StringComparison.Ordinal)

    let empty = String.Empty
    let isNotNullOrEmpty = String.IsNullOrEmpty >> not
    let isNotNullOrWhitespace = String.IsNullOrWhiteSpace >> not

    let visualWidth (s: string) =
        // Fast path: most F# source tokens are pure ASCII, avoid allocating StringInfo.
        let mutable hasNonAscii = false
        let mutable i = 0

        while not hasNonAscii && i < s.Length do
            if s.[i] > '\u007F' then
                hasNonAscii <- true

            i <- i + 1

        if hasNonAscii then
            Globalization.StringInfo(s).LengthInTextElements
        else
            s.Length

module List =
    let chooseState f state l =
        let mutable s = state

        l
        |> List.choose (fun x ->
            let s', r = f s x
            s <- s'
            r
        )

    let isNotEmpty l = (List.isEmpty >> not) l

    let moreThanOne =
        function
        | []
        | [ _ ] -> false
        | _ -> true

    let partitionWhile (f: int -> 'a -> bool) (xs: 'a list) : 'a list * 'a list =
        let rec go i before after =
            match after with
            | [] -> List.rev before, after
            | head :: tail ->

            if f i head then
                go (i + 1) (head :: before) tail
            else
                List.rev before, after

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
        : 'state
        =
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
