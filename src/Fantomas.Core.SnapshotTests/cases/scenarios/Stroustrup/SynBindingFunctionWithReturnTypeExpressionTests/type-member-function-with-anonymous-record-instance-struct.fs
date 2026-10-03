(*---
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = stroustrup
---*)
type Foo() =
    member this.Bar x : {| A:int; B:int; C:int |} =
       struct
            {| A = longTypeName
               B = someOtherVariable
               C = ziggyBarX |}
