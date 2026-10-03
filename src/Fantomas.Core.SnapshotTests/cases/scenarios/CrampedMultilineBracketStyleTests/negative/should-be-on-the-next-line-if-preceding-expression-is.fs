(*---
fsharp_multiline_bracket_style = cramped
---*)
let newDocument = //somecomment
    { program = "Loooooooooooooooooooooooooong"
      content = "striiiiiiiiiiiiiiiiiiinnnnnnnnnnng"
      created = document.Created.ToLocalTime() }
    |> JsonConvert.SerializeObject
