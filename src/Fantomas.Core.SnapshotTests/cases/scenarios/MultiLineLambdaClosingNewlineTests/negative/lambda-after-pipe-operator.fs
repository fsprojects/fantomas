(*---
fsharp_max_infix_operator_expression = 10
fsharp_multi_line_lambda_closing_newline = true
---*)
let printListWithOffset a list1 =
    list1
    |> List.iter (fun elem ->
        // print stuff
        printfn "%d" (a + elem)
    )

let printListWithOffset a list1 =
    list1
    |> List.iter (
        ((+) a)
        >> printfn "%d"
    )
