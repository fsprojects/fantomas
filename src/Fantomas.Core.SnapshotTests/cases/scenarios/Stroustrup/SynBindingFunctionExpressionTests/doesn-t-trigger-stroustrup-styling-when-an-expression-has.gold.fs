let inline skipNoFail count (source: seq<_>) =
    //if FABLE_COMPILER
    seq { yield "Hello" }
