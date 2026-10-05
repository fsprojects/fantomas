(*---
indent_size = 2
fsharp_max_if_then_else_short_width = 9000
---*)
           let name =
                if typ.GenericParameter.IsSolveAtCompileTime then "^" else "'"
                + typ.GenericParameter.Name
