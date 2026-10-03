(*---
max_line_length = 100
fsharp_space_before_uppercase_invocation = true
fsharp_space_before_class_constructor = true
fsharp_space_before_member = true
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
fsharp_max_array_or_list_width = 40
fsharp_align_function_signature_to_indentation = true
fsharp_multi_line_lambda_closing_newline = true
---*)
module Foo =

    let foo () =
        let bar =
            seq {
                for i in ["hello1" ; "hello1" ; "hello1" ; "hello1" ; "hello1"] do
                    yield i, seq {
                        yield "hi"
                        yield "bye"
                    }
            }
        ()
