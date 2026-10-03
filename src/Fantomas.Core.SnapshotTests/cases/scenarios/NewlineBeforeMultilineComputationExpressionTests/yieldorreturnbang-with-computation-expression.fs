(*---
fsharp_max_array_or_list_width = 40
fsharp_newline_before_multiline_computation_expression = false
---*)
myComp {
    yield!
       seq {
            // meh
            return 0 .. 2
       }
    return!
       seq {
            // meh
            return 0 .. 2
       }
}
