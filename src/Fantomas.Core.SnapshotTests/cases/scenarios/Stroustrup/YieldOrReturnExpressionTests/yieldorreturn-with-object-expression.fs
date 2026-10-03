(*---
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = stroustrup
---*)
myComp {
    yield
        { new IFoo with
            member _.Bar() = longTypeName
            member _.Baz() = someOtherVariable }
    return
        { new IFoo with
            member _.Bar() = longTypeName
            member _.Baz() = someOtherVariable }
}
