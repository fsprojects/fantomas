(*---
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = stroustrup
---*)
myComp {
    yield
       struct {| A = longTypeName
                 B =   someOtherVariable
                 C = ziggyBarX |}
    return
        struct
                {| A = longTypeName
                   B = someOtherVariable
                   C = ziggyBarX |}
}
