module Program

/// Parse the command line and run whichever command it names: printing the version, serving the
/// daemon, checking whether files need formatting, or formatting them. Returns the exit code,
/// though most paths through the program end the process themselves rather than returning here.
val main: argv: string array -> int
