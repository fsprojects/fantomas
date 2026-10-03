if not cond then
    ()
elif false then ()
else
    match foo with
    | Some f -> ()
    | None -> printfn "%s" "meh"
