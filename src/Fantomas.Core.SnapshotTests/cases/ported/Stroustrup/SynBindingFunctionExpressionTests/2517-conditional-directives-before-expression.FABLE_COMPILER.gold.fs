let inline skipNoFail count (source: seq<_>) =
    #if FABLE_COMPILER
    seq {
        let mutable i = 0
        let e = source.GetEnumerator()

        while e.MoveNext() do
            if i < count then i <- i + 1 else yield e.Current
    }
#else
#endif
