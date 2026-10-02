(*---
fsharp_max_function_binding_width = 120
---*)
type Person() =
    member this.Walk (distance:int) = ()
    member this.Sleep() = ignore
    member __.singAlong () = ()
    member __.swim (duration:TimeSpan) = ()
