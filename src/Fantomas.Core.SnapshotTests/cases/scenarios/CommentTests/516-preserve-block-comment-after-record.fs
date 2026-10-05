(*---
fsharp_multiline_bracket_style = cramped
---*)
module TriviaModule =

    let env = "DEBUG"

    type Config = {
        Name: string
        Level: int
    }

    let meh = { // this comment right
                                            Name = "FOO"; Level = 78 }

(* ending with block comment *)
