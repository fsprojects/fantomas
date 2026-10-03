(*---
fsharp_space_before_uppercase_invocation = true
---*)
let Meh () = 1

type Bar() =
    member this.Foo(v:int):int = v + 1

let b = Bar()
b |> _.Foo(Meh ())
