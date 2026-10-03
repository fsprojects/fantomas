(*---
fsharp_experimental_keep_indent_in_branch = true
---*)
module Foo =

    let blah =
        
        let a =
            match true with
            | false ->
                match result with
                | Error _ -> failwith ""
                | Ok _ ->
                printfn "hi"
                failwith "blah blah blah blah"
            | true -> failwith ""

        failwith ""
