let sum a b =
    match a with
    | Negative -> None
    | _ ->

    match b with
    | Negative -> None
    | _ ->

    logMessage "a and b are both positive"
    // some grand explainer about the code
    Some(a + b)
