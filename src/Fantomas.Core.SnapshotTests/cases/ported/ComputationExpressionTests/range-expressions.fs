(*---
fsharp_max_infix_operator_expression = 65
fsharp_max_function_binding_width = 65
---*)
let factors number =
    {2L .. number / 2L}
    |> Seq.filter (fun x -> number % x = 0L)