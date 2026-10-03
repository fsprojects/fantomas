async {
    let! (msg: Msg) = inbox.Receive()
    for x in msg.Content do
        printfn "%s" x
    return ()
}

async {
    let! (msg: Msg) = inbox.Receive()

    for x in msg.Content do
        printfn "%s" x

    return ()
}
