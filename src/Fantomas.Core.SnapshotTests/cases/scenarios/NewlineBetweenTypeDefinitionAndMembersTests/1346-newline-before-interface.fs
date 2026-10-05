type andSeq<'t> =
    | AndSeq of 't seq

    interface IEnumerable<'t> with
        member this.GetEnumerator(): Collections.IEnumerator =
            match this with
            | AndSeq xs -> xs.GetEnumerator() :> _
