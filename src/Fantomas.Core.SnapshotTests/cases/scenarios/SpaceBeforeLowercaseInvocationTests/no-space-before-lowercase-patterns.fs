(*---
fsharp_space_before_lowercase_invocation = false
---*)
match x with
| a () -> ()
| B.c () -> ()
| d (e = f) -> ()
| G.h (i = j) -> ()
