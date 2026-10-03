(*---
fsharp_multiline_bracket_style = cramped
---*)
type A =
    | A of int
    | B of {| A: int; LongerThanLengthDeclaration: string|}
