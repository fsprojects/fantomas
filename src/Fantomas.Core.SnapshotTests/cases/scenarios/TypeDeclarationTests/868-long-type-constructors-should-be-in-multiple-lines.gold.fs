type VersionMismatchDuringDeserializationException
    (
        message : string,
        innerException : System.Exception
    ) =
    inherit System.Exception(message, innerException)
