let c =
    if
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
