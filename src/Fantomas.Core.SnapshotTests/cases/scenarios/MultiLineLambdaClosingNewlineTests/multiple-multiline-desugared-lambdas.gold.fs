let myTopLevelFunction v =
    someOtherFunction
        (fun { A = a } ->
            let meh = "foo"
            a
        )
        (fun ({ B = b }) ->
            // probably wrong
            42
        )
        v
