(*---
fsharp_alternative_long_member_definitions = true
---*)
type Foo 
    /// Foo
    [<Attr>] private (x: Looooooooooooooooong, y: Looooooooooooooooong, z: Looooooooooooooooong, a: Looooooooooooooooong, b: Looooooooooooooooong, c) =
    class
        member x.Bar = ()
    end
    member x.Foo = ()
