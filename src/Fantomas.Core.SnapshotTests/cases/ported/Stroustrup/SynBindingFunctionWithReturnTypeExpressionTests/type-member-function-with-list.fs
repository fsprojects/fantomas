(*---
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = stroustrup
---*)
type Foo() =
    member this.Bar x : int list =
        [ itemOne
          itemTwo
          itemThree
          itemFour
          itemFive ]
