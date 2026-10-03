MailboxProcessor<string>.Start
    (fun inbox ->
        async {
            while true do
                let! msg = inbox.Receive()
                do! sw.WriteLineAsync(msg) |> Async.AwaitTask
        })
