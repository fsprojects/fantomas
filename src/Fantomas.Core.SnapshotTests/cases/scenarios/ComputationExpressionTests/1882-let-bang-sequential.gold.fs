async {
    logger.Debug "some message"
    let! token = Async.CancellationToken
    let! model = sendRequest logger credentials token
    return model.Prop
}
