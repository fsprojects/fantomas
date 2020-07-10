namespace Fantomas

/// append only collection optimized for quick append of block of data and query operations
/// data - list of blocks in reverse order
type Queue<'T> (data : list<'T[]>, length : int) = 
    let mutable hashCode = None
    
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

    member this.Head = 
        if length > 0 then (List.head data).[0]
        else raise (System.Exception("Queue is empty"))

    member this.TryHead =
        if length > 0 then Some((List.head data).[0])
        else None
         
    member this.IsEmpty = 
        length = 0

    member this.Length = 
        length

    member this.Rev() = 
        data |> Seq.collect (fun arr -> seq{arr.Length-1 .. -1 .. 0} |> Seq.map (fun i -> arr.[i]))

    member this.Append xs = 
        Queue(Array.ofList xs :: data, length + List.length xs)
    
    /// Equivalent of q |> Queue.toSeq |> Seq.skip n |> Seq.exists f, optimized for speed
    member this.SkipExists n f =
        if n >= length then false else
        let mutable i = length - n // how nany items at end
        let mutable r = false
        let rec dataToEnd acc = function
            | (hd: _[]) :: tl -> 
                if i > hd.Length then
                    i <- i - hd.Length
                    dataToEnd (hd::acc) tl
                else 
                    i <- hd.Length - i // index in first array
                    hd::acc
            | [] -> acc
        let rec exists xs =
            match xs with
            | (arr: _[]) :: tl ->
                while not r && i < arr.Length do
                    if f arr.[i] then r <- true
                    i <- i + 1
                i <- 0
                if r then true else exists tl
            | [] -> r
        let d = dataToEnd [] data
        d |> exists

    interface System.Collections.Generic.IReadOnlyCollection<'T> with
        member this.Count = this.Length
        member this.GetEnumerator() = 
            let e = data |> Seq.rev |> Seq.collect id
            e.GetEnumerator()

        member this.GetEnumerator() = (this :> _ seq).GetEnumerator() :> System.Collections.IEnumerator

[<RequireQualifiedAccess>]
module Queue =
    let empty<'T> : Queue<'T> = Queue<_>([[||]], 0) 

    let inline head (q : Queue<'T>) = q.Head

    let inline tryHead (q : Queue<'T>) = q.TryHead

    let inline isEmpty (q : Queue<'T>) = q.IsEmpty

    let inline length (q : Queue<'T>) = q.Length

    let ofList xs = Queue<'T>([List.toArray xs], List.length xs)

    let ofSeq (xs: seq<_>) = Queue<'T>([Seq.toArray xs], Seq.length xs)

    let inline rev (q : Queue<'T>) = q.Rev()

    let inline toSeq (q: Queue<'T>) = q :> seq<'T>

    let inline append (q : Queue<'T>) xs = q.Append xs

    /// Equivalent of q |> Queue.toSeq |> Seq.skip n |> Seq.exists f
    let inline skipExists n f (q : Queue<'T>) = q.SkipExists n f
