(*---
fsharp_max_infix_operator_expression = 75
fsharp_multiline_bracket_style = cramped
---*)
let newDocument = //somecomment
    { program = Encoding.Default.GetBytes(document.Program) |> Encoding.UTF8.GetString
      content = Encoding.Default.GetBytes(document.Content) |> Encoding.UTF8.GetString
      created = document.Created.ToLocalTime() }
    |> JsonConvert.SerializeObject
