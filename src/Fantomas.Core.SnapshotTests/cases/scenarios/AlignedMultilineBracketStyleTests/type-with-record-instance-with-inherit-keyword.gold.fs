type ServerCannotBeResolvedException =
    inherit CommunicationUnsuccessfulException

    new(message) =
        {
            inherit CommunicationUnsuccessfulException(message)
        }
