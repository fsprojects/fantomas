(*---
fsharp_max_array_or_list_width = 40
fsharp_newline_before_multiline_computation_expression = false
---*)
Bar
    .Foo(fun x ->
                    task {
                        // some computation here
                        ()
                    }).Bar()
