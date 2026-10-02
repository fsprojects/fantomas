(*---
# Inline block comment stays single-line - expected output needs review
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = stroustrup
---*)
let list = [
    someItem (*
      trivia!
    *)
]
