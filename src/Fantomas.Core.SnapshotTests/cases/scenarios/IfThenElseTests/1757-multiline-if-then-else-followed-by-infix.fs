(*---
indent_size = 2
---*)
           let name =
                if typ.GenericParameter.IsSolveAtCompileTime then "^" else "'"
                + typ.GenericParameter.Name
