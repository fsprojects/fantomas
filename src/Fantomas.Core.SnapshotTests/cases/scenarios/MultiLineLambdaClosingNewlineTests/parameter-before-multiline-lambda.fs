(*---
fsharp_multi_line_lambda_closing_newline = true
---*)
let mySuperFunction a =
    someOtherFunction a (fun b ->
        // doing some stuff her
       b * b
    )
