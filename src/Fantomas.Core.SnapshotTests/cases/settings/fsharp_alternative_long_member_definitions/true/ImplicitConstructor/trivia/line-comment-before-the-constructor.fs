(*---
fsharp_alternative_long_member_definitions = true
---*)
type Foo
    // comment
    (x:int) =
    member _.X = x
