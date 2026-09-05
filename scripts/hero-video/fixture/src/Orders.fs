module Orders

open Domain

type Order =
    | Pending of Customer * amount : decimal
    | Shipped   of Customer
    | Cancelled

let total orders = orders |> List.sumBy (fun o -> match o with | Pending (_, amount) -> amount | _ -> 0m)

let describe = function
    | Pending (c, a) -> sprintf "%s owes %M" c.Name a
    | Shipped c ->   sprintf "%s: shipped" c.Name
    | Cancelled -> "cancelled"
