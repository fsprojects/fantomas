let c =
    if blah then
        true
    elif
        bar
        |> Seq.exists (
            (|KeyValue|)
            >> snd
            >> (=) (Some i)
        )
    then
        false
    else
        true
