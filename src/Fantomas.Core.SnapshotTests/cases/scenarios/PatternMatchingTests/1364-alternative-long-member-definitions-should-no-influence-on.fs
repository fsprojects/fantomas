(*---
max_line_length = 100
fsharp_newline_between_type_definition_and_members = false
fsharp_alternative_long_member_definitions = true
---*)
type Thing =
| Foo of msg : string
with
    override this.ToString () =
        match this with
        | Foo (ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff) ->
            ""
