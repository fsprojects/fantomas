(*---
fsharp_record_multiline_formatter = number_of_items
fsharp_multiline_bracket_style = cramped
---*)
let a =
    [
        { new System.Object() with member x.ToString() = "F#" }
        { new System.Object() with member x.ToString() = "C#" }
    ]
