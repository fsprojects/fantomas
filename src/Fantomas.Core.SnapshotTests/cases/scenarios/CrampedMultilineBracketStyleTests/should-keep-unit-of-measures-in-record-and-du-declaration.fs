(*---
fsharp_multiline_bracket_style = cramped
---*)
type rate = {Rate:float<GBP*SGD/USD>}
type rate2 = Rate of float<GBP/SGD*USD>
