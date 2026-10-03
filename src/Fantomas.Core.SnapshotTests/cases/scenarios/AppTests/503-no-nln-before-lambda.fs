(*---
max_line_length = 80
fsharp_max_infix_operator_expression = 40
---*)
let a =
    b
    |> List.exists (fun p ->
        p.a && p.b |> List.exists (fun o -> o.a = "lorem ipsum dolor sit amet"))
    