(*---
fsharp_multiline_bracket_style = cramped
---*)
let internal sepSemi (ctx: Context) =
    let { Config = { SpaceBeforeSemicolon = before; SpaceAfterSemicolon = after } } = ctx

    match before, after with
    | false, false -> str ";"
    | true, false -> str " ;"
    | false, true -> str "; "
    | true, true -> str " ; "
    <| ctx
