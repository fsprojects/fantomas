(*---
fsharp_blank_lines_around_nested_multiline_expressions = false
---*)
let topLevelFunction () =
    printfn "Something to print"
    try
            nothing ()
    with
    | ex ->
        splash ()
    ()
