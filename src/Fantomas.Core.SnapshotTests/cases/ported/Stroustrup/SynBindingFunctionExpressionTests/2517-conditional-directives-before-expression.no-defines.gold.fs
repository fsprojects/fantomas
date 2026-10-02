let inline skipNoFail count (source: seq<_>) =
    #if FABLE_COMPILER
    #else
    Enumerable.Skip(source, count)
#endif
