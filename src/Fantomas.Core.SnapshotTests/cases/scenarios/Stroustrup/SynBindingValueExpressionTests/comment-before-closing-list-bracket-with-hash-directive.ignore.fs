(*---
# Trivia ordering broken when comment follows #endif - see comment above
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = stroustrup
---*)
let list = [
    someItem
    #if something
    item1
    #else
    item2
    #endif
    // comment
                ]
