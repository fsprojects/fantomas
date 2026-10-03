Program.statefulWithCmdMsg
|> (fun program ->
            { program with
                CanReuseView = ViewHelper.canReuseView
                SyncAction =
                    (fun fn ->
                        program.SyncAction
                            (
#if IOS
                            // iOS animates by default layout changes, we don't want that
                            fun () -> UIKit.UIView.PerformWithoutAnimation(fn)
#else
                            fn
#endif
                        )) })
