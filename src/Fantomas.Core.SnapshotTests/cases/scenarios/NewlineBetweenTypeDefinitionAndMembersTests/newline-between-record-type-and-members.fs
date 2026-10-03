(*---
fsharp_max_value_binding_width = 120
fsharp_multiline_bracket_style = cramped
---*)
type Range =
    { From : float
      To : float
      Name: string }
    member this.Length = this.To - this.From
