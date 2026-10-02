let blah<'a> config : Type =
    //#if DEBUG
    failwith ""
    //#endif
    DoThing.doIt ()
    let result = Runner.Run<'a> config
    ()
