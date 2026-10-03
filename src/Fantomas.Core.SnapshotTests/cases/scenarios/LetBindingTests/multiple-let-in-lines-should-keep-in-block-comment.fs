(*---
fsharp_max_value_binding_width = 50
---*)
let f () =
  let x = 1 in   (* the "in" keyword is available in F# *)
    let y = 2 in
      x + y
