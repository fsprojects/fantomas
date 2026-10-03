type Shell() =
    static member private GetParams(cmd, ?args) = doStuff
    static member Exec(cmd, ?args) = shellExec (Shell.GetParams(cmd, ?args = args))
