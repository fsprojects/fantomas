let watchFiles =
    async {
        printfn "after start"

        use _ =
            !!(serverPath </> "*.fs") ++ "*.fsproj" // combines fs and fsproj
            |> ChangeWatcher.run (fun changes ->
                printfn "FILE CHANGE %A" changes
                // stopFunc()
                //Async.Start (startFunc())
            )

        ()
    }
