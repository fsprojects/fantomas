(*---
fsharp_multiline_bracket_style = cramped
---*)
type Car = {
    Make : string
    Model : string
    mutable Odometer : int
    }

let myRecord3 = { myRecord2 with Y = 100; Z = 2 }