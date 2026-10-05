let foo () =
    async {
        match!
            b.TryGetValue(
                longlonglonglonglong
            )
        with
        | true, i -> Some i
        | false, _ -> failwith ""
    }
