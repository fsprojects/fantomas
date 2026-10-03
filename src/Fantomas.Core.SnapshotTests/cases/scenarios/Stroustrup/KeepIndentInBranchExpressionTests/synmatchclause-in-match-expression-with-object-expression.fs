(*---
fsharp_max_array_or_list_width = 40
fsharp_experimental_keep_indent_in_branch = true
fsharp_multiline_bracket_style = stroustrup
---*)
match x with
| _ ->
    { new IFoo with
        member _.Bar() = longTypeName
        member _.Baz() = someOtherVariable }
