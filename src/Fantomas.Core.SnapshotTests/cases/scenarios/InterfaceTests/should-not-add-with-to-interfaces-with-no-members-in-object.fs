(*---
fsharp_max_value_binding_width = 120
fsharp_multiline_bracket_style = cramped
---*)
let f () =
    { new obj() with
        member x.ToString() = "INotifyEnumerableInternal"
      interface INotifyEnumerableInternal<'T>
      interface IEnumerable<_> with
        member x.GetEnumerator() = null }