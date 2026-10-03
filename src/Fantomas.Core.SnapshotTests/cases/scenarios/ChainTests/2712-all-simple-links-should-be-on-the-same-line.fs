(*---
max_line_length = 45
---*)
type Duck() =
    member this.Duck  = Duck ()
    member this.Goose() = Duck()
    
let d = Duck()

d.Duck.Duck.Duck.Goose().Duck.Goose().Duck.Duck.Goose().Duck.Duck.Duck.Goose().Duck.Duck.Duck.Duck.Goose()
