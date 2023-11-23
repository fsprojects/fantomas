namespace Fantomas.Core

/// append only collection optimized for quick append of block of data and query operations
/// data - list of blocks in reverse order
type Queue<'T>(data: 'T array list, length: int) =
    let mutable hashCode = None

    override x.GetHashCode() =
        match hashCode with
        | None ->
            let mutable hash = 1

            for x' in x do
                hash <- 31 * hash + Unchecked.hash x'

            hashCode <- Some hash
            hash
        | Some hash -> hash

    override x.Equals(other) =
        match other with
        | :? Queue<'T> as y ->
            if x.Length <> y.Length then false
            else if x.GetHashCode() <> y.GetHashCode() then false
            else Seq.forall2 Unchecked.equals x y
        | _ -> false

    member x.Head =
        if length > 0 then
            (List.head data).[0]
        else
            raise (System.Exception("Queue is empty"))

    member x.TryHead = if length > 0 then Some((List.head data).[0]) else None

    member x.Tail =
        match data with
        | [] -> x
        | head :: tail ->
            if Array.isEmpty head then
                x
            else
                let newHead = Array.skip 1 head

                if Array.isEmpty newHead then
                    Queue(tail, length - 1)
                else
                    Queue(newHead :: tail, length - 1)

    member x.IsEmpty = length = 0

    member x.Length = length

    member x.Rev() =
        data
        |> Seq.collect (fun arr -> seq { arr.Length - 1 .. -1 .. 0 } |> Seq.map (fun i -> arr.[i]))

    member x.Append xs =
        Queue(Array.ofList xs :: data, length + List.length xs)

    /// Equivalent of q |> Queue.toSeq |> Seq.skip n |> Seq.skipWhile p |> Seq.exists f, optimized for speed
    member x.SkipExists n f p =
        if n >= length then
            false
        else
            let mutable i = length - n // how many items at end
            let mutable r = false

            let rec dataToEnd acc =
                function
                | hd: _ array :: tl ->
                    if i > hd.Length then
                        i <- i - hd.Length
                        dataToEnd (hd :: acc) tl
                    else
                        i <- hd.Length - i // index in first array
                        hd :: acc
                | [] -> acc

            let rec exists xs =
                match xs with
                | arr: _ array :: tl ->
                    while not r && i < arr.Length do
                        if f arr.[i] then
                            r <- true

                        i <- i + 1

                    i <- 0
                    if r then true else exists tl
                | [] -> r

            let d = dataToEnd [] data
            d |> List.skipWhile p |> exists

    interface System.Collections.Generic.IReadOnlyCollection<'T> with
        member x.Count = x.Length

        member x.GetEnumerator() =
            let e = data |> Seq.rev |> Seq.collect id
            e.GetEnumerator()

        member x.GetEnumerator() =
            (x :> _ seq).GetEnumerator() :> System.Collections.IEnumerator

[<RequireQualifiedAccess>]
module Queue =
    let empty<'T> : Queue<'T> = Queue<_>([ [||] ], 0)

    let inline head (q: Queue<'T>) = q.Head

    let inline tryHead (q: Queue<'T>) = q.TryHead

    let inline isEmpty (q: Queue<'T>) = q.IsEmpty

    let inline length (q: Queue<'T>) = q.Length

    let ofList xs =
        Queue<'T>([ List.toArray xs ], List.length xs)

    let ofSeq (xs: _ seq) =
        Queue<'T>([ Seq.toArray xs ], Seq.length xs)

    let inline rev (q: Queue<'T>) = q.Rev()

    let inline toSeq (q: Queue<'T>) = q :> 'T seq

    let inline append (q: Queue<'T>) xs = q.Append xs

    /// Equivalent of q |> Queue.toSeq |> Seq.skip n |> Seq.skipWhile p |> Seq.exists f
    let inline skipExists (n: int) (f: 'T -> bool) (p: 'T array -> bool) (q: Queue<'T>) : bool = q.SkipExists n f p
