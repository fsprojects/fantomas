(*---
fsharp_multiline_bracket_style = cramped
---*)
let compareThings (first: Thing) (second: Thing) =
    first = { second with
                Foo = first.Foo
                Bar = first.Bar
            }
