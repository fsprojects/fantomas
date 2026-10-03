(*---
fsharp_max_function_binding_width = 120
---*)
type Shell() =
    static member private GetParams(cmd, ?args) = doStuff
    static member Exec(cmd, ?args) =
        shellExec(Shell.GetParams(cmd, ?args = args))

    