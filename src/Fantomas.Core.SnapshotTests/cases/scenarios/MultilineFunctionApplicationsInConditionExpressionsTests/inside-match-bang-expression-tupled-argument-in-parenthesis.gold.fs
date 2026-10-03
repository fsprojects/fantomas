let foo () =
    async {
        match!
            b.TryGetValue(
                longlonglonglonglong,
                b
            )
        with
        | true, i -> Some i
        | false, _ -> failwith ""
    }
