(*---
fsharp_max_function_binding_width = 120
fsharp_newline_between_type_definition_and_members = false
fsharp_multiline_bracket_style = cramped
---*)
type Route =
    { Verb : string
      Path : string
      Handler : Map<string, string> -> HttpListenerContext -> string }
    override x.ToString() = sprintf "%s %s" x.Verb x.Path

    