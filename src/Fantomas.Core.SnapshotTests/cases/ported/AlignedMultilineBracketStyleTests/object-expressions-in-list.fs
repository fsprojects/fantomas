(*---
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
---*)
let a =
    [
        { new System.Object() with member x.ToString() = "F#" }
        { new System.Object() with member x.ToString() = "C#" }
    ]
