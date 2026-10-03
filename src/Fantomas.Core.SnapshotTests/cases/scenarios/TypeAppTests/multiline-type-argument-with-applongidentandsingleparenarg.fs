(*---
max_line_length = 30
fsharp_space_before_uppercase_invocation = true
fsharp_space_before_class_constructor = true
fsharp_space_before_member = true
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
fsharp_newline_between_type_definition_and_members = false
fsharp_align_function_signature_to_indentation = true
fsharp_alternative_long_member_definitions = true
fsharp_multi_line_lambda_closing_newline = true
---*)
path.Replace<
        Foo<
            'innerContextLongLongLong,
            'bb -> 'b
         >
     >("../../../", "....")
