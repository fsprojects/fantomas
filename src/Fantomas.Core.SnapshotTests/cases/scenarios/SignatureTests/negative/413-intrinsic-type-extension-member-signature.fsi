(*---
fsharp_newline_between_type_definition_and_members = false
---*)
namespace ExtensionParts

type T =
    new: unit -> T

type T with
    member Foo: int
