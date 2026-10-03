(*---
fsharp_multiline_bracket_style = cramped
---*)
let private fn (xs: int[]) =
    fn2
        ""
        [ let r = Seq.head xs

          yield r

          let s = fn2()
          s.DoSomething() ]
