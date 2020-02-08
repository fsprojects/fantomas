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
        
    let private lengthWithoutSpaces (str: string) =
        normalizeNewLine str
        |> fun s -> s.Replace("\n", String.Empty).Replace(" ", String.Empty)
        |> String.length

    let private hashRegex = @"^\s*#(if|elseif|else|endif).*"
    let private splitWhenHash (source: string) = 
        source.Split([| Environment.NewLine; "\r\n"; "\n" |], options = StringSplitOptions.None)
        |> Array.fold (fun acc line ->
            if Regex.IsMatch(line, hashRegex) then
                let trimmmedLine = line.TrimStart()
                match acc with
                | [[]] -> [[trimmmedLine]]
                | _ -> [trimmmedLine]::acc
            else
                acc
                |> List.mapi (fun idx l -> if idx = 0 then (line::l) else l)
        ) [[]]
        |> List.map (List.rev >> String.concat Environment.NewLine)
        |> List.rev

    let merge a b =
        let aChunks = splitWhenHash a
        let bChunks = splitWhenHash b
        
        if List.length aChunks <> List.length bChunks then
            Dbg.print (aChunks, bChunks)
            failwithf """Fantomas is trying to format the input multiple times due to the detect of multiple defines.
There is a problem with merging all the code back togheter. Please raise an issue at https://github.com/fsprojects/fantomas/issues."""
        
        List.zip aChunks bChunks
        |> List.map (fun (a', b') ->
            let la = lengthWithoutSpaces a'
            let lb = lengthWithoutSpaces b'
            if la <> lb then 
                if la > lb then a' else b'
            else
                if String.length a' < String.length b' then a' else b' 
        )
        
        |> String.concat Environment.NewLine

    let empty = System.String.Empty

module Cache =
    let alreadyVisited<'key when 'key : not struct>() =
        let cache = System.Collections.Generic.HashSet<'key>([], HashIdentity.Reference)
        fun key ->
            if cache.Contains key then
                true
            else
                cache.Add key |> ignore
                false
                
    let memoizeBy (g: 'a -> 'c) (f: 'a -> 'b) =
        let cache = System.Collections.Concurrent.ConcurrentDictionary<_, _>(HashIdentity.Structural)
        fun x ->
            cache.GetOrAdd(Some (g x), lazy (f x)).Force()
            
    [<CustomEquality; NoComparison>]
    type LambdaEqByRef<'a,'b> = LambdaEqByRef of ('a -> 'b) with
        override this.Equals(obj) =
            match obj with
            | :? LambdaEqByRef<'a,'b> as y ->
                let (LambdaEqByRef f) = this
                let (LambdaEqByRef g) = y
                Object.ReferenceEquals(f,g)
            | _ -> false
        override this.GetHashCode() = 0
        
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

    let isNotEmpty l = (List.isEmpty >> not) l

module Reflection =
    open FSharp.Reflection
    let inline getRecordFields x =
        let names = FSharpType.GetRecordFields (x.GetType()) |> Seq.map (fun x -> x.Name)
        let values = FSharpValue.GetRecordFields x
        Seq.zip names values |> Seq.toArray

/// FIFO queue, from https://github.com/fsprojects/FSharpx.Collections/blob/master/src/FSharpx.Collections/Queue.fs        
type Queue<'T> (front : list<'T>, rBack : list<'T>) = 
    let mutable hashCode = None
    member internal this.front = front
    member internal this.rBack = rBack

    override this.GetHashCode() =
        match hashCode with
        | None ->
            let mutable hash = 1
            for x in this do
                hash <- 31 * hash + Unchecked.hash x
            hashCode <- Some hash
            hash
        | Some hash -> hash

    override this.Equals(other) =
        match other with
        | :? Queue<'T> as y -> 
            if this.Length <> y.Length then false 
            else
                if this.GetHashCode() <> y.GetHashCode() then false
                else Seq.forall2 (Unchecked.equals) this y
        | _ -> false

    member this.Conj x = 
        match front, x::rBack with
        | [], r -> Queue((List.rev r), [])
        | f, r -> Queue(f, r)

    member this.Head = 
        match front with
        | hd::_ -> hd
        | _ -> raise (new System.Exception("Queue is empty"))

    member this.TryHead =
        match front with
        | hd::_ -> Some(hd)
        | _ -> None
         
    member this.IsEmpty = front.IsEmpty

    member this.Length = front.Length + rBack.Length

    member this.Rev() = 
        match rBack, front with
        | [], r -> Queue((List.rev r), [])
        | f, r -> Queue(f, r)

    member this.Tail =
        match front with
        | hd::tl -> 
            match tl, rBack with
            | [], r -> Queue((List.rev r), [])
            | f, r -> Queue(f, r)
        | _ -> raise (new System.Exception("Queue is empty"))
            
    member this.TryTail =
        match front with
        | hd::tl ->
            match tl, rBack with
            | [], r -> Some(Queue((List.rev r), []))
            | f, r -> Some(Queue(f, r))
        | _ -> None

    member this.Uncons =  
        match front with
        | hd::tl -> 
            hd, (match tl, rBack with
                | [], r -> Queue((List.rev r), [])
                | f, r -> Queue(f, r))
        | _ -> raise (new System.Exception("Queue is empty"))

    member this.TryUncons =  
        match front with
        | hd::tl -> 
            match tl, rBack with
            | [], r -> Some(hd, Queue((List.rev r), []))
            | f, r -> Some(hd, Queue(f, r))
        | _ -> None

    interface System.Collections.Generic.IReadOnlyCollection<'T> with
        member this.Count = this.Length
        member this.GetEnumerator() = 
            let e = seq {
                  yield! front
                  yield! (List.rev rBack)}
            e.GetEnumerator()

        member this.GetEnumerator() = (this :> _ seq).GetEnumerator() :> System.Collections.IEnumerator

[<RequireQualifiedAccess>]
module Queue =
    //pattern discriminators  (active pattern)
    let (|Cons|Nil|) (q : Queue<'T>) = match q.TryUncons with Some(a,b) -> Cons(a,b) | None -> Nil

    let inline conj (x : 'T) (q : Queue<'T>) = (q.Conj x) 

    let empty<'T> : Queue<'T> = Queue<_>([], []) 

    let fold (f : ('State -> 'T -> 'State)) (state : 'State) (q : Queue<'T>) = 
        let s = List.fold f state q.front
        List.fold f s (List.rev q.rBack)

    let foldBack (f : ('T -> 'State -> 'State)) (q : Queue<'T>) (state : 'State) =  
        let s = List.foldBack f (List.rev q.rBack) state 
        (List.foldBack f q.front s)

    let inline head (q : Queue<'T>) = q.Head

    let inline tryHead (q : Queue<'T>) = q.TryHead

    let inline isEmpty (q : Queue<'T>) = q.IsEmpty

    let inline length (q : Queue<'T>) = q.Length

    let ofList xs = Queue<'T>(xs, [])

    let ofSeq xs = Queue<'T>((List.ofSeq xs), [])

    let inline rev (q : Queue<'T>) = q.Rev()

    let inline tail (q : Queue<'T>) = q.Tail

    let inline tryTail (q : Queue<'T>) = q.TryTail

    let inline toSeq (q: Queue<'T>) = q :> seq<'T>

    let inline uncons (q : Queue<'T>) = q.Uncons

    let inline tryUncons (q : Queue<'T>) = q.TryUncons