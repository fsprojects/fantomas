(*---
# An exception is a single union case, and still gets no bar.
fsharp_bar_before_discriminated_union_declaration = true
---*)
exception LoadedSourceNotFoundIgnoring of string * range (*filename*)
