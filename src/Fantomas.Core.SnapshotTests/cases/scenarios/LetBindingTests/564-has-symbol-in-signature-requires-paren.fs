(*---
fsharp_space_before_parameter = false
fsharp_space_after_comma = false
fsharp_space_after_semicolon = false
fsharp_space_around_delimiter = false
---*)
module Bar =
  let foo (_ : #(int seq)) = 1
  let meh (_: #seq<int>) = 2
  let evenMoreMeh (_: #seq<int>) : int = 2
