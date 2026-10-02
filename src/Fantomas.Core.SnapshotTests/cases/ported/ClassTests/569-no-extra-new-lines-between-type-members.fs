(*---
fsharp_max_value_binding_width = 120
---*)
type A() =

    member this.MemberA = if true then 0 else 1

    member this.MemberB = if true then 2 else 3

    member this.MemberC = 0