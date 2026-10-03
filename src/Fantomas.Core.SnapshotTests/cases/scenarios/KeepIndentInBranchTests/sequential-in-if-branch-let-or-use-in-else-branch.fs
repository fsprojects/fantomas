(*---
fsharp_experimental_keep_indent_in_branch = true
---*)
open Library

type RunMode =
    | Dry
    | Wet

let main argv =
    let args = parse argv

    let instructions = Library.foo args
    if args.DryRun = RunMode.Dry then
        printfn "Would execute actions, but --dry-run was supplied: %+A" instructions
        0
    else
    // proceed with main method
    let output = Library.execute instructions
    // do more stuff
    0
