(*---
fsharp_space_before_colon = true
---*)
type OuterType =
    abstract Apply<'r>
        : InnerType<'r>
        -> 'r when 'r : comparison
