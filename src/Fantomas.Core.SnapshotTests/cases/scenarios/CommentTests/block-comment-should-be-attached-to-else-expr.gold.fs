let compilerOptionUsage (CompilerOption(s, tag, spec, _, _)) =
    let s =
        if s = "--" then
            ""
        else
            s (* s="flag" for "--flag" options. s="--" for "--" option. Adjust printing here for "--" case. *)

    s
