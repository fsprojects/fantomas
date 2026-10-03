(*---
fsharp_max_array_or_list_width = 40
fsharp_newline_before_multiline_computation_expression = false
---*)
try
    foo()
with
| ex ->
    task {
        // some computation here
        ()
    }
