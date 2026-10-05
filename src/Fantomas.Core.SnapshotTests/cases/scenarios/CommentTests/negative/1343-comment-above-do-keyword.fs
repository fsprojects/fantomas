if stateSub.Value |> State.hasChanges then
    // Push changes
    let! pushed = push state

    // Import new etags of pushed items
    do
        pushed
        |> Option.bind Dto.changesAsImport
        |> Option.iter (fun changes -> update (Import changes) |> ignore)
