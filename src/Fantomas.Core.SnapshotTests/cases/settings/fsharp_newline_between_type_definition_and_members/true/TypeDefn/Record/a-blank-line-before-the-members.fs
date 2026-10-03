(*---
fsharp_newline_between_type_definition_and_members = true
---*)
type Point =
    {
        X: int
        Y: int
    }
    member p.Sum = p.X + p.Y
