(*---
fsharp_max_function_binding_width = 120
---*)
module ``member``

let ``abstract`` = "abstract"

type SomeType() =
    member this.``new``() =
        System.Console.WriteLine("Hello World!")
    