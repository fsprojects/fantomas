(*---
fsharp_max_if_then_else_short_width = 50
---*)
let internal coli f' (c: seq<'T>) f (ctx: Context) =
    let mutable tryPick = true
    let mutable st = ctx
    let mutable i = 0
    let e = c.GetEnumerator()

    while (e.MoveNext()) do
        if tryPick then tryPick <- false else st <- f' st
        st <- f i (e.Current) st
        i <- i + 1

    st
