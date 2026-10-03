(*---
max_line_length = 100
fsharp_space_before_uppercase_invocation = true
fsharp_space_before_class_constructor = true
fsharp_space_before_member = true
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
fsharp_max_infix_operator_expression = 50
fsharp_align_function_signature_to_indentation = true
fsharp_alternative_long_member_definitions = true
fsharp_multi_line_lambda_closing_newline = true
fsharp_experimental_keep_indent_in_branch = true
---*)
let main (args : Options) =
    log.LogDebug ("Command line options: {Options}", args.ToString())

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
