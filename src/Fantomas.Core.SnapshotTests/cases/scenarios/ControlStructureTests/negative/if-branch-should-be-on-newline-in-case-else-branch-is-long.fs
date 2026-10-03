if not cond then
    ()
else
    match foo with
    | Some f -> ()
    | None -> printfn "%s" "meh"
