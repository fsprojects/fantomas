type CustomerId =
    | CustomerId of int
    member this.Test() = printfn "%A" this
