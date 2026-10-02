let sum a b =
    match a, b with
    | Negative _, _
    | _, Negative _ -> None
    | a, b ->

    logMessage "a and b are both positive"
    // some grand explainer about the code
    Some(a + b)
