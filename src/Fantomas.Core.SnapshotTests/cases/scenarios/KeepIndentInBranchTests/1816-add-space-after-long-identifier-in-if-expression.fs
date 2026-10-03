(*---
max_line_length = 100
fsharp_max_infix_operator_expression = 50
fsharp_multi_line_lambda_closing_newline = true
fsharp_experimental_keep_indent_in_branch = true
---*)
module Foo =
    let assertConsistent () : unit =
        if veryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryLong then
            ()
        else
            if foo = bar then
                ()
            else
            let leftSet = HashSet (FooBarBaz.keys leftThings)
            leftSet.SymmetricExceptWith (FooBarBaz.keys rightThings)
            |> ignore
