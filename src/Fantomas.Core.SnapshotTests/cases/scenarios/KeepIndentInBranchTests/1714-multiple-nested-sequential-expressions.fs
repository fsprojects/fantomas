(*---
fsharp_experimental_keep_indent_in_branch = true
---*)
namespace Foo

module Bar =

    [<EntryPoint>]
    let main argv =
        let args = foo
        printfn ""
        printfn ""
        printfn ""
        let m = ""
        if foo then
            printfn "aborting"
            1
        else

        printfn "blah"
        let m = ""
        if foo then
            printfn "aborting"
            1
        else

        let fs = FileSystem ()
        use f = fs.File.Open("")
        0
