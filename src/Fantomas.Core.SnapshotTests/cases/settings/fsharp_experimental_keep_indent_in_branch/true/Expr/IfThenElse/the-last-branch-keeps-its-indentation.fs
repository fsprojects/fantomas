(*---
fsharp_experimental_keep_indent_in_branch = true
---*)
let validate input =
    if String.IsNullOrWhiteSpace input then
        Error "empty"
    else
    let trimmed = input.Trim ()
    Ok   trimmed
