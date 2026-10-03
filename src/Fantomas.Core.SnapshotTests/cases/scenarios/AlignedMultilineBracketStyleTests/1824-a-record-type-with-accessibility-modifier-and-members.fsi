(*---
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
---*)
namespace Thing

type Foo =
    private
        {
            Bar : int
            Qux : string
        }
    static member Baz : int
