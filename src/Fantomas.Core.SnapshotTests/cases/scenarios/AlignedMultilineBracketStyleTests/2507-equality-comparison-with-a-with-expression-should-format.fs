(*---
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
---*)
let compareThings (first: Thing) (second: Thing) =
    first = { second with
                Foo = first.Foo
                Bar = first.Bar
            }
