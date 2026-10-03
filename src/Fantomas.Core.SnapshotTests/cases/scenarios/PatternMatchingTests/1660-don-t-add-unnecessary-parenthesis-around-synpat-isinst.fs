(*---
fsharp_max_if_then_else_short_width = 40
fsharp_max_infix_operator_expression = 50
---*)
        match other with
        | :? Queue<'T> as y ->
            if this.Length <> y.Length then
                false
            else if this.GetHashCode() <> y.GetHashCode() then
                false
            else
                Seq.forall2 Unchecked.equals this y
        | _ -> false
