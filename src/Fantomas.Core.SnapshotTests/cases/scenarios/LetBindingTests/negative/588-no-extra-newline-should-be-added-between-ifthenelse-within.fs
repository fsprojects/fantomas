(*---
fsharp_max_if_then_short_width = 50
---*)
let x =
    if true then printfn "a"
    elif true then printfn "b"

    if true then 1 else 0
