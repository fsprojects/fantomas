let compilerOptionUsage (CompilerOption (s, tag, spec, _, _)) =
    let s =
        if s = "--" then
            ""
        else
            s (* s="flag" for "--flag" options. s="--" for "--" option. Adjust printing here for "--" case. *)

    match spec with
    | OptionUnit _
    | OptionSet _
    | OptionClear _
    | OptionHelp _ -> sprintf "--%s" s
    | OptionStringList _ -> sprintf "--%s:%s" s tag
    | OptionIntList _ -> sprintf "--%s:%s" s tag
    | OptionSwitch _ -> sprintf "--%s[+|-]" s
    | OptionStringListSwitch _ -> sprintf "--%s[+|-]:%s" s tag
    | OptionIntListSwitch _ -> sprintf "--%s[+|-]:%s" s tag
    | OptionString _ -> sprintf "--%s:%s" s tag
    | OptionInt _ -> sprintf "--%s:%s" s tag
    | OptionFloat _ -> sprintf "--%s:%s" s tag
    | OptionRest _ -> sprintf "--%s ..." s
    | OptionGeneral _ ->
        if tag = "" then
            sprintf "%s" s
        else
            sprintf "%s:%s" s tag (* still being decided *)
