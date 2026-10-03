(*---
fsharp_newline_between_type_definition_and_members = false
---*)
type C() =
  class
   member x.P = 1
  end
  with
    member _.Run() = 1
