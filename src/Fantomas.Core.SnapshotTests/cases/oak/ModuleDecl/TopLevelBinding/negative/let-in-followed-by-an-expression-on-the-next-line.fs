(*---
# At module level the line after `in` is a declaration of its own, not the body of the binding.
---*)
let x = 1 in
printfn "%d" x
