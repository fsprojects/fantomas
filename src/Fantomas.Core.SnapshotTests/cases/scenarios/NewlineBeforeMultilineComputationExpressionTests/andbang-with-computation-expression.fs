(*---
fsharp_max_array_or_list_width = 40
fsharp_newline_before_multiline_computation_expression = false
---*)
task {
    let! abc = def ()
    and! meh =
        task {
            // comment
            return 42
        }
    ()
}
