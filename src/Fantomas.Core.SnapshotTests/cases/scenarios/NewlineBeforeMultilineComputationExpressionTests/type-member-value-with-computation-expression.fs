(*---
fsharp_max_array_or_list_width = 40
fsharp_newline_before_multiline_computation_expression = false
---*)
type Foo() =
    member this.Bar =
        task {
            // some computation here
            ()
        }
