(*---
fsharp_newline_between_type_definition_and_members = false
---*)
type X = A
and Y = B
    with
        [<ExcludeFromCodeCoverage>]
        member  this.M() = true
