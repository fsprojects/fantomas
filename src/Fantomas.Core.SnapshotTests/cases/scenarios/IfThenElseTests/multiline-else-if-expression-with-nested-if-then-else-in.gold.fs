if result.LaunchSuccess && result.ExitCode = 0 then
    Ok r
else if result.ExitCode = 1 then
    let stdout, stderr =
        output
        |> List.map (function
            | StdErr e -> Error e
            | StdOut l -> Ok l)
        |> Result.partition

    if not stderr.IsEmpty then
        failwithf "Got stderr bad bad bad"

    match stdout with
    | [] -> failwithf "Got no stdout :("
    | xs when
        xs
        |> List.exists (fun i -> i.Contains "magic string goes here!")
        ->
        Error(Ok r)
    | _ -> Error(Error r)
else
    failwith ""
