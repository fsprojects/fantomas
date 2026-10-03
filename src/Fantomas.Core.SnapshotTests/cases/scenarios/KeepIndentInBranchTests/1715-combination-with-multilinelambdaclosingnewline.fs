(*---
fsharp_space_before_uppercase_invocation = true
fsharp_space_before_class_constructor = true
fsharp_space_before_member = true
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
fsharp_max_infix_operator_expression = 50
fsharp_align_function_signature_to_indentation = true
fsharp_alternative_long_member_definitions = true
fsharp_multi_line_lambda_closing_newline = true
fsharp_experimental_keep_indent_in_branch = true
---*)
module Foo =
    let bar () =
        let baz =
            []
            |> List.filter (fun ref ->
                if ref.Type <> "h" then false
                else
                let m = regex.Match ref.To
                m.Success && things |> Set.contains (m.Groups.[1].ToString ())
            )
        0
