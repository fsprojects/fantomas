(*---
fsharp_experimental_keep_indent_in_branch = true
---*)
module Program =
    let main _ =
        if false then 1 else
        printfn "hi!"
        0
