let initDb () =
    if not (File.Exists(dbFileName)) then
        let dbFile = File.Create(dbFileName)
        dbFile.Dispose() |> ignore

    let createSql = readSqlFile "create"

    using (connection ()) (fun conn ->
        task {
            do! conn.OpenAsync()
            let! _ = conn.ExecuteAsync(createSql)
#if DEBUG
            let! hasClients = hasClients ()

            if not (hasClients) then
                let seedSql = readSqlFile "seed"
                let! _ = conn.ExecuteAsync(seedSql)
                ()
#else
            ()
#endif
        })
