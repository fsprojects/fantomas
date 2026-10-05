(*---
fsharp_space_before_semicolon = true
fsharp_space_after_semicolon = false
---*)
type MyExc =
    inherit Exception
    new(msg) = { inherit Exception(msg); X = 1; }
