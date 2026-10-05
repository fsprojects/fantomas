let cts = new CancellationTokenSource()

let mb =
    MailboxProcessor.Start(
        fun inbox ->
            let rec messageLoop _ = async { return! messageLoop () }

            messageLoop ()
        , cts.Token
    )
