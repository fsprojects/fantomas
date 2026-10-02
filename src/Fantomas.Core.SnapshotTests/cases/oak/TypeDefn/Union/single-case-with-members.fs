(*---
# A single case stays below the type name when the union has members.
---*)
type CustomerId =
    | CustomerId of int
    member this.Test() =
        printfn "%A" this
