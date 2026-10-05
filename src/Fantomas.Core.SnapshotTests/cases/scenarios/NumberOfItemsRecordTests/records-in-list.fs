(*---
fsharp_record_multiline_formatter = number_of_items
fsharp_multiline_bracket_style = cramped
---*)
let configurations =
    [
        { Build = true; Configuration = "RELEASE"; Defines = ["FOO"] }
        { Build = true; Configuration = "DEBUG"; Defines = ["FOO";"BAR"] }
        { Build = true; Configuration = "UNKNOWN"; Defines = [] }
    ]
