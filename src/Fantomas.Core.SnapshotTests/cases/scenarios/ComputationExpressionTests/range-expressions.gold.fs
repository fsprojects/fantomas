let factors number = { 2L .. number / 2L } |> Seq.filter (fun x -> number % x = 0L)
