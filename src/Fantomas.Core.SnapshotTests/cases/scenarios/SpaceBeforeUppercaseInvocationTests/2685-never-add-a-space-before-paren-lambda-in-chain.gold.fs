module A =
    let foo =
        Foai
            .SomeLongTextYikes()
            .ConfigureBarry(fun alpha beta gamma -> context.AddSomething ("a string") |> ignore)
            .MoreContext(fun builder ->
                // also good stuff
                ())
            .ABC()
            .XYZ
