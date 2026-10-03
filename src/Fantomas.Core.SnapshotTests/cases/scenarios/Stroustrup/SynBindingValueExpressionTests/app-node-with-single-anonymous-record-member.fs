(*---
fsharp_record_multiline_formatter = number_of_items
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = stroustrup
---*)
let newState = {|
    Foo =
        Some
            {|
                F1 = 0
                F2 = ""
            |}
|}
