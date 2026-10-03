(*---
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
---*)
let myRecord =
    { Level = 1
      Progress = "foo"
      Bar = { Zeta = "bar" }
      Address =
          { Street = "Bakerstreet"
            ZipCode = "9000" }
      Number = 42 }
