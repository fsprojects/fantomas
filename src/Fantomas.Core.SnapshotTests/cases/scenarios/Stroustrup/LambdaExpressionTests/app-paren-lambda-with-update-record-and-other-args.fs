(*---
max_line_length = 60
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = stroustrup
---*)
List.map (fun x ->
    { astContext with IsInsideMatchClausePattern = true }) b c
