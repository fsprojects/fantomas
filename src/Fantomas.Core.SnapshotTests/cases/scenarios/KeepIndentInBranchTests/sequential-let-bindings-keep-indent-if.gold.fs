let main (args : Options) =
    log.LogDebug ("Command line options: {Options}", args.ToString ())

    let includes =
        if ArgParser.defaultArg args.Flag then
            Flag.Include
        else
            Flag.Exclude

    if dryRunMode then
        log.LogInformation ("No changes made due to --dry-run.")
        0
    else

    match requested with
    | None ->
        log.LogWarning ("No changes required; no action taken.")
        0
    | Some branched ->

    branched
    |> blah
    |> fun i -> log.LogInformation ("Done:\n{It}", i)

    0
