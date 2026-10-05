let ((userId, _), events) = request

task {
    do! EventStore.appendEvents userId events
    return sendText "Events persisted"
}
