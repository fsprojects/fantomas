(*---
fsharp_multiline_bracket_style = cramped
---*)
let expect =
    Result<int,string>.Ok [ "fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo"
                            "baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar"
                            "meh"
                          ]