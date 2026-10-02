(*---
fsharp_space_before_lowercase_invocation = false
fsharp_space_before_colon = true
fsharp_max_if_then_else_short_width = 25
fsharp_max_infix_operator_expression = 50
fsharp_align_function_signature_to_indentation = true
fsharp_alternative_long_member_definitions = true
fsharp_multi_line_lambda_closing_newline = true
---*)
let inline (=??) x = (=!) x
let mySampleMethod() =
    let result = Ok {| Results = [] |}
    (Result.okValue result).Results.[0] |> Result.isOk =?? true
