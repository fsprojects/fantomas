(*---
fsharp_multiline_bracket_style = stroustrup
---*)
{| payload with reason = reason |}
|> Json.serialize<{| reason: string; old: bool; ``new``: bool |}>
