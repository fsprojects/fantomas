(*---
fsharp_max_infix_operator_expression = 50
---*)
match structuralTypes |> List.tryFind (fst >> checkIfFieldTypeSupportsComparison tycon >> not) with
| _ -> ()
