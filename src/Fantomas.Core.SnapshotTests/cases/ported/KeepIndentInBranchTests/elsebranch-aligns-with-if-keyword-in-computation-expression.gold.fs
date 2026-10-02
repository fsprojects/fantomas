async {
    if not (proc.Start()) then
        return Error "failed to start"
    else

    use stdout = proc.StandardOutput
    let! ct = Async.CancellationToken
    return! Async.AwaitTask(stdout.ReadToEndAsync ct)
}
