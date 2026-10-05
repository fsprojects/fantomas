(*---
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = stroustrup
---*)
fun x ->
    { astContext with IsInsideMatchClausePattern = true }
