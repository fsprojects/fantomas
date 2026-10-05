if foo then
    ()
elif bar then
    match foo with
    | Some f -> ()
    | None -> printfn "%s" "meh"
else
    ()
