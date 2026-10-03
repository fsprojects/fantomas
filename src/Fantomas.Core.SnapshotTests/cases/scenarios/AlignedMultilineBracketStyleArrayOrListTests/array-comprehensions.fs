(*---
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
fsharp_max_if_then_short_width = 25
---*)
let a1 = [| 0 .. 99 |]
let a2 = [| for n in 1 .. 100 do if isPrime n then yield n |]