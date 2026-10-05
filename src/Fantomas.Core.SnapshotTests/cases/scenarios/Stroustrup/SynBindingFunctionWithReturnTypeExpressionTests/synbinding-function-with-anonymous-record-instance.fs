(*---
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = stroustrup
---*)
let x y : {| A:int; B:int; C:int |} =
    {| A = longTypeName
       B = someOtherVariable
       C = ziggyBarX |}
