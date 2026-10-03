(*---
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
fsharp_max_value_binding_width = 120
---*)
type Range =
    { From: float
      To: float }
    member this.Length = this.To - this.From
