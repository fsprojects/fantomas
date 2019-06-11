namespace Fantomas

open System

[<RequireQualifiedAccess>]
module String =
    let normalizeNewLine (str : string) =
        str.Replace("\r\n", "\n").Replace("\r", "\n")

    let normalizeThenSplitNewLine (str : string) =
        (normalizeNewLine str).Split('\n')

    let startsWithOrdinal (prefix : string) (str : string) =
        str.StartsWith(prefix, StringComparison.Ordinal)

    let removeTrailingSpaces (source:string) =
        source.Split([| Environment.NewLine |], StringSplitOptions.None)
        |> Array.map (fun line -> line.TrimEnd())
        |> fun lines -> String.Join(Environment.NewLine, lines)
        |> fun code -> code.TrimStart(Environment.NewLine.ToCharArray())

module Cache =
    let alreadyVisited<'key when 'key : not struct>() =
        let cache = System.Collections.Generic.HashSet<'key>([], HashIdentity.Reference)
        fun key ->
            if cache.Contains key then
                true
            else
                cache.Add key |> ignore
                false

module Dict =
    let tryGet k (d: System.Collections.Generic.IDictionary<_,_>) =
        let (r,x) = d.TryGetValue k
        if r then Some x else None
        
module List =
    let appendItem l i =
        l @ [i]
        
    let prependItem l i = i :: l
    
    let takeWhileState f state l =
        let mutable s = state
        l |> List.takeWhile (fun x -> let (s',r) = f s x in s <- s'; r)