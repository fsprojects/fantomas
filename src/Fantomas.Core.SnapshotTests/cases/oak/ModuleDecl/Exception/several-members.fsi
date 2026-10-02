(*---
# In a signature file the members are member signatures, built by a different path.
---*)
module M

exception BuildException of string * string list with
    override Message: string
    member Errors: string list
    static member Create: msg: string -> exn
