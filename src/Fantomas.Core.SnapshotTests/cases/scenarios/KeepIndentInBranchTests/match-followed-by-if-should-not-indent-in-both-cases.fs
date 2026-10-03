(*---
fsharp_experimental_keep_indent_in_branch = true
---*)
let sum a b =
    if a < 0 then
        None
    else
    logMessage "a is positive"
    match b with
    | Negative -> None
    | _ ->
    logMessage "a and b are both positive"
    // some grand explainer about the code
    Some (a + b)
