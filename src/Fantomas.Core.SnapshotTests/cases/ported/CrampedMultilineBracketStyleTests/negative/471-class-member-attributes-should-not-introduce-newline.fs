(*---
fsharp_multiline_bracket_style = cramped
---*)
type Test =
    | String of string

    [<SomeAttribute>]
    member x._Print = ""

    member this.TestMember = ""
