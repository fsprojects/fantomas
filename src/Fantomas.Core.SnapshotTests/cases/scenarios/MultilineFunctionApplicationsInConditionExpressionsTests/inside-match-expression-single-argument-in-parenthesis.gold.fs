let foo () =
    match
        b.TryGetValue(
            longlonglonglonglong
        )
    with
    | true, i -> Some i
    | false, _ -> failwith ""
