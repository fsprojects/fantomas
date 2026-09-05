module Program

open Domain
open Orders

[<EntryPoint>]
let main argv =
    let alice = { Id = 7; Name = "Alice"; Email = "alice@example.vip" }
    let orders = [ Pending(alice, 42m); Shipped alice ]
    printfn "%s" (greet alice)
    printfn "Total: %M" (total orders)
    0
