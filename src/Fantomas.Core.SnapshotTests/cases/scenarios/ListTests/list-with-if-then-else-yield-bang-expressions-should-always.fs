(*---
fsharp_max_if_then_else_short_width = 120
fsharp_max_array_or_list_width = 120
---*)
let value = [
    if foo then yield! ["a";"b"] else yield "c"
    if bar then yield "d" else yield! ["e";"f"]
]
