(*---
fsharp_bar_before_discriminated_union_declaration = true
---*)
type Foo =   | [<SomeAttributeHere>] Foo of int
