namespace Fantomas

/// FIFO queue, from https://github.com/fsprojects/FSharpx.Collections/blob/master/src/FSharpx.Collections/Queue.fs        
//[<CustomEquality; NoComparison>]
type Queue<'T> (data : list<'T[]>, length : int) = 
    let mutable hashCode = None
    member internal this.Data = data

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

    // member this.Conj x = 
    //     front.Insert(length, x)
    //     Queue(front, length + 1)

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

    // // member this.Tail =
    //     match front with
    //     | _::tl ->
    //         match tl, rBack with
    //         | [], r -> Queue((List.rev r), [])
    //         | f, r -> Queue(f, r)
    //     | _ -> raise (System.Exception("Queue is empty"))
            
    // member this.TryTail =
    //     match front with
    //     | _::tl ->
    //         match tl, rBack with
    //         | [], r -> Some(Queue((List.rev r), []))
    //         | f, r -> Some(Queue(f, r))
    //     | _ -> None

    // member this.Uncons =  
    //     match front with
    //     | hd::tl -> 
    //         hd, (match tl, rBack with
    //             | [], r -> Queue((List.rev r), [])
    //             | f, r -> Queue(f, r))
    //     | _ -> raise (System.Exception("Queue is empty"))

    // member this.TryUncons =  
    //     match front with
    //     | hd::tl -> 
    //         match tl, rBack with
    //         | [], r -> Some(hd, Queue((List.rev r), []))
    //         | f, r -> Some(hd, Queue(f, r))
    //     | _ -> None

    member this.Append xs = 
        Queue(Array.ofList xs :: data, length + List.length xs)

    member this.SkipExists n f =
        // let l = front.Length
        // if l > n then
        //     (front |> List.skip n |> List.exists f) || (rBack |> List.rev |> List.exists f)
        // else rBack |> List.rev |> List.skip (n - l) |> List.exists f
        let data = data |> List.toArray
        let dataLen = data.Length
        let mutable r = false
        let mutable i = n 
        let mutable k = dataLen-1
        while k >= 0 && i >= data.[k].Length do
            i <- i - data.[k].Length
            k <- k - 1
        while k >= 0 && r=false && i < data.[k].Length do
            if f data.[k].[i] then r <- true
            if i < data.[k].Length-1 then i <- i + 1
            else
                i <- 0
                k <- k - 1
        r

    // member this.RevTakeWhile f =
    //     seq{length .. -1 .. 0} |> Seq.map (fun i -> front.[i]) |> Seq.takeWhile f

    // member this.RevSkipWhileTryHead f =
    //     seq{length .. -1 .. 0} |> Seq.map (fun i -> front.[i]) |> Seq.skipWhile f |> Seq.tryHead

    interface System.Collections.Generic.IReadOnlyCollection<'T> with
        member this.Count = this.Length
        member this.GetEnumerator() = 
            let e = data |> Seq.rev |> Seq.collect id
            e.GetEnumerator()

        member this.GetEnumerator() = (this :> _ seq).GetEnumerator() :> System.Collections.IEnumerator

[<RequireQualifiedAccess>]
module Queue =
    //pattern discriminators  (active pattern)
    //let (|Cons|Nil|) (q : Queue<'T>) = match q.TryUncons with Some(a,b) -> Cons(a,b) | None -> Nil

    // let inline conj (x : 'T) (q : Queue<'T>) = (q.Conj x) 

    let empty<'T> : Queue<'T> = Queue<_>([[||]], 0) 

    // let fold (f : ('State -> 'T -> 'State)) (state : 'State) (q : Queue<'T>) = 
    //     let s = Seq.fold f state q.Front
    //     s

    // let foldBack (f : ('T -> 'State -> 'State)) (q : Queue<'T>) (state : 'State) =  
    //     (Seq.foldBack f q.Front state)

    let inline head (q : Queue<'T>) = q.Head

    let inline tryHead (q : Queue<'T>) = q.TryHead

    let inline isEmpty (q : Queue<'T>) = q.IsEmpty

    let inline length (q : Queue<'T>) = q.Length

    let ofList xs = Queue<'T>([List.toArray xs], List.length xs)

    let ofSeq (xs: seq<_>) = Queue<'T>([Seq.toArray xs], Seq.length xs)

    let inline rev (q : Queue<'T>) = q.Rev()

    // let inline tail (q : Queue<'T>) = q.Tail

    // let inline tryTail (q : Queue<'T>) = q.TryTail

    let inline toSeq (q: Queue<'T>) = q :> seq<'T>

    // let inline uncons (q : Queue<'T>) = q.Uncons

    // let inline tryUncons (q : Queue<'T>) = q.TryUncons

    let inline append (q : Queue<'T>) xs = q.Append xs

    /// Equivalent of q |> Queue.toSeq |> Seq.skip n |> Seq.exists f
    let inline skipExists n f (q : Queue<'T>) = q.SkipExists n f

    // let revTakeWhile f (q : Queue<'T>) =
    //     q.RevTakeWhile f