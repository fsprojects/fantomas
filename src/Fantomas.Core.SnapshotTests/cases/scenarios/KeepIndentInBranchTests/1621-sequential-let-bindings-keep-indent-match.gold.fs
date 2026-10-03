let main (args : Options) =
    log.LogDebug ("Command line options: {Options}", args.ToString ())

    let includes =
        if ArgParser.defaultArg args.Flag then
            Flag.Include
        else
            Flag.Exclude

    match dryRunMode with
    | DryRunMode.Dry ->
        log.LogInformation ("No changes made due to --dry-run.")
        0
    | DryRunMode.Wet ->

    match requested with
    | None ->
        log.LogWarning ("No changes required; no action taken.")
        0
    | Some branched ->

    branched
    |> blah
    |> fun i -> log.LogInformation ("Done:\n{It}", i)

    0
