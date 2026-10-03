(*---
fsharp_multiline_bracket_style = cramped
---*)
(sli.Dots, tail)
||> List.zip
|> List.collect (fun (dot, ident) ->
    [ IdentifierOrDot.KnownDot(stn "." dot)
      IdentifierOrDot.Ident(mkSynIdent ident) ])
