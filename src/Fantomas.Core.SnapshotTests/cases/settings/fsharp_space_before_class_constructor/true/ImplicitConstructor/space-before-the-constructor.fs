(*---
fsharp_space_before_class_constructor = true
---*)
type Person(name: string) =
    member _.Name = name
