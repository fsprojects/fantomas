(*---
fsharp_multiline_bracket_style = cramped
---*)
match entities with
| [ { Key = "0031ff53-e59b-49e3-8e0f-53f72a3890d1"
      Type = Elephant }
    { Key = "0031ff53-e59b-49e3-8e0f-53f72a3890d2"
      Type = Elephant }
    { Key = "0031ff53-e59b-49e3-8e0f-53f72a3890d3"
      Type = Elephant } ] -> ()
| _ -> ()
