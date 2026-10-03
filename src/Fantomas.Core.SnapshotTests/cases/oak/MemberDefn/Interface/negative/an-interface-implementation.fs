type Resource() =
    interface System.IDisposable with
        member _.Dispose() = ()
