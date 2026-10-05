(*---
fsharp_max_function_binding_width = 120
---*)
namespace global

type SomeType() =
    member this.Print() =
        global.System.Console.WriteLine("Hello World!")
    