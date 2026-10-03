if cond then
    match foo with
    | Some f -> ()
    | None -> printfn "%s" "meh"
else
    ()
