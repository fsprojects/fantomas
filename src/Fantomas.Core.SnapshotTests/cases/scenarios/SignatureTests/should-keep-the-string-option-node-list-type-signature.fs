(*---
fsharp_multiline_bracket_style = cramped
---*)
type Node =
    { Name : string;
      NextNodes : (string option * Node) list }

    