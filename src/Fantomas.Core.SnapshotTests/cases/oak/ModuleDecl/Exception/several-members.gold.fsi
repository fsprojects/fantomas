module M

exception BuildException of string * string list with
    override Message: string
    member Errors: string list
    static member Create: msg: string -> exn
