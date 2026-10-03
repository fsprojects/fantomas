exception BuildException of string * string list with
    override x.Message = x.Data0
    member x.Errors = x.Data1
    static member Create(msg: string) = BuildException(msg, [])
