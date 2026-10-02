(*---
fsharp_space_before_uppercase_invocation = true
fsharp_space_before_member = true
fsharp_multiline_bracket_style = cramped
---*)
let blah =
    Mock<Foo>()
        .Returns (fun _ ->
            { dasdasdsadsadsadsa = ""
              Sdadsadasdasdas = "sdsadsadasdsa" })
