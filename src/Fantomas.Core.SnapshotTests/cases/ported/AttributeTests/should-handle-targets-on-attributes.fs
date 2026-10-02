(*---
fsharp_multiline_bracket_style = cramped
---*)
[<DataContract>]
type Foo =
    { [<field:DataMember>]
      Bar:string }
