(*---
fsharp_newline_between_type_definition_and_members = false
fsharp_multiline_bracket_style = stroustrup
---*)
type V =
    { X: SomeFieldType
      Y: OhSomethingElse
      Z: ALongTypeName }
    member this.Coordinate = (this.X, this.Y, this.Z)
