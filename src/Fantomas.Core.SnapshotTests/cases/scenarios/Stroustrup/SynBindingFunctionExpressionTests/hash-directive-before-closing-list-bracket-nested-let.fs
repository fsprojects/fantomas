(*---
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = stroustrup
---*)
let foo bar =
    let tfms = [
    #if NET6_0_OR_GREATER
        "net6.0"
    #endif
    #if NET7_0_OR_GREATER
        "net7.0"
    #endif
                                ]
    ()
