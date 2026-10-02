(*---
# In a signature file the members are member signatures, built by a different path. `with` moves up
# to the line of the case.
---*)
module M

exception BuildException of string * string list
    with
    override Message : string
    member Errors : string list
    static member Create : msg: string -> exn
