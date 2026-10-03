(*---
max_line_length = 40
fsharp_space_before_uppercase_invocation = true
---*)
let c =
    if blah then
        true
    elif bar |> Seq.exists ((|KeyValue|) >> snd >> (=) (Some i)) then false else true
