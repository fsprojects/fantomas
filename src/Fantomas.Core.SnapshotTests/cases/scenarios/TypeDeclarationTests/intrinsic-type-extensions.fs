(*---
fsharp_newline_between_type_definition_and_members = false
---*)
type MyClass() =
      member this.F() = 100

type MyClass with
    member this.G() = 200