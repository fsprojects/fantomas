match meh with
| OptionGeneral _ ->
    if tag = "" then
        sprintf "%s" s
    else
        sprintf "%s:%s" s tag (* still being decided *)
