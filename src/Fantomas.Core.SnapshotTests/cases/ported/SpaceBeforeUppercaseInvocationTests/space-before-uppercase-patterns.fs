(*---
fsharp_space_before_uppercase_invocation = true
---*)
match x with
| A() -> ()
| b.C() -> ()
| D(e = f) -> ()
| g.H(i = j) -> ()
