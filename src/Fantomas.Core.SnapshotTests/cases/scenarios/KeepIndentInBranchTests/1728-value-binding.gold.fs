let x =
    if
        not (
            result.HasResultsFor(
                [ "label"
                  "ipv4"
                  "macAddress"
                  "medium"
                  "manufacturer" ]
            )
        )
    then
        None
    else

    let label = string result.["label"]
    let ipv4 = string result.["ipv4"]
    None
