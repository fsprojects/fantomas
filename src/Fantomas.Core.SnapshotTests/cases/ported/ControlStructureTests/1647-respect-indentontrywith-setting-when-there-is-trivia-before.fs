(*---
fsharp_space_before_uppercase_invocation = true
fsharp_space_before_class_constructor = true
fsharp_space_before_member = true
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
fsharp_align_function_signature_to_indentation = true
fsharp_alternative_long_member_definitions = true
fsharp_multi_line_lambda_closing_newline = true
fsharp_experimental_keep_indent_in_branch = true
---*)
module Foo =
    let blah () =
        match foo with
        | Thing crate ->

        crate.Apply
            { new Evaluator<_, _> with
                member __.Eval inner teq =
                    let foo =
                        // blah
                        let exists =
                            try
                                let defaultTime =
                                    (DateTime.FromFileTimeUtc 0L).ToLocalTime ()

                                foo.CreationTime <> defaultTime
                            with
                            // hmm
                            :? FileNotFoundException -> false

                        exists

                    ()
            }
