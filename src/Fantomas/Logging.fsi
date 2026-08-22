module Fantomas.Logging

[<RequireQualifiedAccess>]
type VerbosityLevel =
    | Normal
    | Detailed

/// Initialise the logger for command line use.
/// Informational messages are written to standard out, warnings and errors to standard error,
/// so a caller can tell the tool's output apart from its diagnostics by stream.
val initLogger: level: VerbosityLevel -> VerbosityLevel

/// Initialise the logger for daemon mode, where standard out carries the JSON-RPC protocol.
/// Everything is written to standard error, because a single log line on standard out would
/// corrupt the protocol stream and fault the client connection.
val initDaemonLogger: level: VerbosityLevel -> VerbosityLevel

/// Flush anything the logger is still holding and shut it down.
val closeAndFlushLog: unit -> unit
