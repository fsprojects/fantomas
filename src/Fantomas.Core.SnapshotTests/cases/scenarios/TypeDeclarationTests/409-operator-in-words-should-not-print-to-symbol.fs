(*---
fsharp_space_before_member = true
fsharp_max_function_binding_width = 120
---*)
type T() =
    static member op_LessThan(a, b) = a < b