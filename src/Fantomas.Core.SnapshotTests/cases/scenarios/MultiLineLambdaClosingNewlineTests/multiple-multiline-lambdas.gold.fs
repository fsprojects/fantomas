let mySuperFunction v =
    someOtherFunction
        (fun a ->
            let meh = "foo"
            a
        )
        (fun b ->
            // probably wrong
            42
        )
        v
