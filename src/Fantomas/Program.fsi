module Program

/// Parse the command line and run whichever command it names: printing the version, serving the
/// daemon, checking whether files need formatting, or formatting them. Returns the exit code the
/// process should end with.
val main: argv: string array -> int
