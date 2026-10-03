Program.statefulWithCmdMsg
|> (fun program ->
    { program with
        CanReuseView = ViewHelper.canReuseView
        SyncAction =
            (fun fn ->
                program.SyncAction(
                    #if IOS
                    #else
                    fn
                #endif
                ))
    })
