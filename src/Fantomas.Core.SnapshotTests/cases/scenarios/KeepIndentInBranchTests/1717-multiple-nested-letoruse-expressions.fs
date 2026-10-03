(*---
fsharp_alternative_long_member_definitions = true
fsharp_multi_line_lambda_closing_newline = true
fsharp_experimental_keep_indent_in_branch = true
---*)
module Foo =
    let main (args : _) =
        let fs = FileSystem()
        let ab = ThingOne.make fs (ThingFour.defaultFoo args.Args.ThingOne)

        let thingFive = ThingFive.thingFive fs ab

        use loggerFactory = Logging.make thingFive LogEventLevel.Verbose LogEventLevel.Information
        let log = loggerFactory.CreateLogger "foo"
        log.LogDebug("Command line options used: {CommandLine}", args)
        log.LogInformation("Thing One: {ThingOne}", ThingOne.getFoo ab)

        let thing, cd =
            args.Thing
            |> Args.render loggerFactory thingFive

        let skipBehaviour =
            if defaultArg args.Skip then
                Options.Exclude
            else
                Options.Include

        let thing, errors = Information.make fs ab thing
        use thing = thing

        let operationToDo =
            Operations.operation loggerFactory fs ab tp thing (DUCase args.Thing) args.Info skipBehaviour

        match ThingEight.defaultFun args.DryRun with
        | DryRunMode.Dry ->
            log.LogInformation("No changes made due to --dry-run.")
            0
        | DryRunMode.Wet ->

        match operationToDo with
        | None ->
            log.LogWarning("No changes required; no action taken.")
            0
        | Some thingsToDo ->

        thingsToDo
        |> Operations.perform loggerFactory (errors |> Map.keys)
        |> Seq.map (fun i -> i.Name)
        |> String.concat "\n"
        |> fun i -> log.LogInformation("Completed operation:\n{Result}", i)
        0
