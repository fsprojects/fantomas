(*---
fsharp_max_infix_operator_expression = 50
---*)
let a = List.init 40 (fun i -> generateThing i a) |> List.map mapThingToOtherThing