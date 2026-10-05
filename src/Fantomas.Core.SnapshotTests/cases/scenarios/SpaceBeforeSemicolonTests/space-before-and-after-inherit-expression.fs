(*---
fsharp_space_before_semicolon = true
---*)
type MyExc =
    inherit Exception
    new(msg) = { inherit Exception(msg); X = 1; }
