let m =
    match x with
    | y -> ErrorMessage msg
    | _ -> LogMessage(msg, true)
    |> console.Write
    