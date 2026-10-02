(*---
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = stroustrup
---*)
let x y =
    { new IFoo with
        member _.Bar() = longTypeName
        member _.Baz() = someOtherVariable }
