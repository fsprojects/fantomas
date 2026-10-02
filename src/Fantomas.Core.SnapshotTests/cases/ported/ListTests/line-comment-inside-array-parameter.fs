(*---
fsharp_space_around_delimiter = false
fsharp_multiline_bracket_style = cramped
---*)
let prismCli commando =
    let props =
        createObj [|
            "component" ==> "pre"
            //"className" ==> "language-fsharp"
        |]
    ()
