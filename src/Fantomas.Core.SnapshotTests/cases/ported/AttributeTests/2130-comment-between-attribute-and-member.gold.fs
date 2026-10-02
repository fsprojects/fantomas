type StreamReaderExtensions =
    [<Extension>]
    // Extension method for StreamReader using Tasks to enable specifying timeout.
    // Returns `None` if timeout occurs, otherwise `Some string`.
    // Beware that since StreamReader.ReadLineAsync task can't be cancelled,
    // it might still be running after recieving None!
    static member inline Meh(streamReader: StreamReader, timeout: TimeSpan) = ()
