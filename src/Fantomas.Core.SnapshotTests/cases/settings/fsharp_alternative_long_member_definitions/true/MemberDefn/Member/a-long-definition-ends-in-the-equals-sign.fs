(*---
fsharp_alternative_long_member_definitions = true
---*)
type Repository() =
    member this.CreateCustomer(firstName: string, lastName: string, emailAddress: string, phoneNumber: string, address: string) : int = 42
