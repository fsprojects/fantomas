(*---
fsharp_max_infix_operator_expression = 50
---*)
let elifs =
    es
    |> List.collect (fun (e1, e2, _, _, _) -> [ visit e1; visit e2 ])
