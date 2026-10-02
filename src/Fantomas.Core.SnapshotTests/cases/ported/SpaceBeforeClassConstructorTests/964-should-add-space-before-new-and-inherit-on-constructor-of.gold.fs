type ProtocolGlitchException =
    inherit CommunicationUnsuccessfulException

    new (message) = { inherit CommunicationUnsuccessfulException (message) }

    new (message: string, innerException: Exception) =
        { inherit CommunicationUnsuccessfulException (message, innerException) }
