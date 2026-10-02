(*---
fsharp_max_infix_operator_expression = 50
---*)
let v = // <- Lazy "1"
    lazy
        "123456798123456798123456798"
        |> idLongFunctionThing
        |> string