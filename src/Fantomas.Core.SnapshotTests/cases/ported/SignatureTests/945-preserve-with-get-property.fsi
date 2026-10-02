(*---
fsharp_space_before_colon = true
fsharp_newline_between_type_definition_and_members = false
---*)
namespace B
type Foo =
    | Bar of int
    member Item : unit -> int with get
