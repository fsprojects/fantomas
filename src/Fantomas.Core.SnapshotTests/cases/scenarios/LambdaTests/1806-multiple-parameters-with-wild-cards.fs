(*---
fsharp_max_infix_operator_expression = 50
fsharp_multiline_bracket_style = cramped
---*)
module Foo =
    let bar () =
        {
            Foo =
                blah
                |> Struct.map (fun _ (a, _, _) -> filterBackings a)
        }
