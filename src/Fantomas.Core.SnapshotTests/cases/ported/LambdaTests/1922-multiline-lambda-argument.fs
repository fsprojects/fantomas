(*---
fsharp_multiline_bracket_style = cramped
---*)
let g =
    Array.groupBy
        (fun { partNumber = p
               revisionNumber = r
               processName = pn } -> p, r, pn)
