(*---
# Formatting drops the `struct` of the constraint, and the result does not parse.
---*)
let inline retype (x: ^T) : ^U = (# "" x : ^U #) when ^T struct = 0
