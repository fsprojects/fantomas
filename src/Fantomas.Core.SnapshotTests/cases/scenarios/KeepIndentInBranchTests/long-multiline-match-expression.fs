(*---
max_line_length = 100
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
    let bar =

        match output.Exists && not (output.EnumerateFiles() |> Seq.isEmpty && onlyContainsFoobar output) with
        | true ->
            Error (FooBarBazError.ErrorCase output)
        | false ->

        let blah =
            let x = y
            None

        {
            Hi = blah
        }
        |> Ok
