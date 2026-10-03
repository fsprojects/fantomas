let disposable =
    { new System.IDisposable with
        member _.Dispose() = ()
      interface System.IComparable with
          member _.CompareTo _ = 0
    }
