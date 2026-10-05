(*---
fsharp_blank_lines_around_nested_multiline_expressions = false
---*)
let topLevelFunction () =
    let innerValue = 23
    let innerMultilineFunction () =
        // some comment
        printfn "foo"
    ()
