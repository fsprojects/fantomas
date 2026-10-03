(*---
fsharp_experimental_keep_indent_in_branch = true
---*)
module Foo =

    let main (args : _) =
        let thing1 = ()
        printfn ""

        match instructions with
        | Error e ->
            printfn ""
            2
        | Ok (thing, instructions) ->

        log.LogInformation("")
        match Something.foo args with
        | DryRunMode.Dry ->
            printfn ""
            0
        | DryRunMode.Wet ->

        Thing.execute
            bar
            baz
            (thing, instructions)
        0
