module Domain

type Customer = { Id : int ; Name : string;
                  Email : string }

let greet (c:Customer) =
    sprintf "Hello, %s!"    c.Name

let isVip customer = customer.Id < 100 && customer.Email.EndsWith ".vip"
