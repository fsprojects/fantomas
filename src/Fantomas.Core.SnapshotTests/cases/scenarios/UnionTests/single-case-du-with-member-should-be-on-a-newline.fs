(*---
fsharp_max_function_binding_width = 120
fsharp_newline_between_type_definition_and_members = false
---*)
type CustomerId =
    | CustomerId of int
    member this.Test() =
        printfn "%A" this
    