match other with
| :? Queue<'T> as y ->
    if this.Length <> y.Length then
        false
    else if this.GetHashCode() <> y.GetHashCode() then
        false
    else
        Seq.forall2 Unchecked.equals this y
| _ -> false
