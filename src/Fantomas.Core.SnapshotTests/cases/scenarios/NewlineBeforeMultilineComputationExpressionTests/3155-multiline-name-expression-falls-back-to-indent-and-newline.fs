(*---
max_line_length = 30
fsharp_max_array_or_list_width = 40
fsharp_newline_before_multiline_computation_expression = false
---*)
let a = Builder.build ("a long enough str") {
    let b = 1
    return b
}
