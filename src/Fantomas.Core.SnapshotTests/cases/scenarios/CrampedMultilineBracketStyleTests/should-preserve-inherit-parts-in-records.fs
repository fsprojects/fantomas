(*---
fsharp_multiline_bracket_style = cramped
---*)
type MyExc =
    inherit Exception
    new(msg) = {inherit Exception(msg)}
