(*---
fsharp_max_if_then_else_short_width = 40
---*)
let f x =
    React.useEffect (fun () ->
        if length x > 5 && length x < 10 then
            doX x
        else
            doY x
    , [| x |])

    ()
