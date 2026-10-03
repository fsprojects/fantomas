(*---
fsharp_newline_between_type_definition_and_members = false
fsharp_multiline_bracket_style = cramped
---*)
namespace Thing

type Foo =
    private
        {
            Bar : int
            Qux : string
        }
    static member Baz : int
