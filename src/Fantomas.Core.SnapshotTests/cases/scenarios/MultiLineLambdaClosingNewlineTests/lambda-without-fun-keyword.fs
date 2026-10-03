(*---
fsharp_max_infix_operator_expression = 5
---*)
let printListWithOffset a list1 =
    List.iter (
        ((+) a)
        >> printfn "%d"
    ) list1
