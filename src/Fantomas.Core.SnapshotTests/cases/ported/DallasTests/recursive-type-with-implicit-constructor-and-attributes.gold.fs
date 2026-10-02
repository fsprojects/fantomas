type x

and [<Sealed>] MapDebugView<'Key, 'Value when 'Key: comparison>(v: Map<'Key, 'Value>) =

    [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
    member x.Items =
        v |> Seq.truncate 10000 |> Seq.map KeyValuePairDebugFriendly |> Seq.toArray
