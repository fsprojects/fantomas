(*---
# The comment takes the place of the blank line a long group gets before `and`.
---*)
let rec f x =
    match x with
    | 0 -> false
    | _ -> g (x - 1)
// comment before and
and g x =
    match x with
    | 0 -> true
    | _ -> f (x - 1)
