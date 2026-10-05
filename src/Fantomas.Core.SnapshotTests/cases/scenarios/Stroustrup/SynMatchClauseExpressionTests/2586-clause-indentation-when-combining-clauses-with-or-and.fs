(*---
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = stroustrup
---*)
let y x =
    match x with
        | Case1
        | Case2 -> [ "X" ]
        | Case3 -> [ "Y" ]
