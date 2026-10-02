(*---
fsharp_experimental_keep_indent_in_branch = true
---*)
let foo =
    bar
    |> List.filter (fun { Index = i } ->
        if false then
            false
        else

        let m = quux
        quux.Success && somethingElse
    )
