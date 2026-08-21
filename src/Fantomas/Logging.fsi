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

/// log a message
val stdlog: s: string -> unit

/// log an error
val elog: s: string -> unit

/// log a message if the verbosity level is >= Detailed
val logGrEqDetailed: s: string -> unit

val closeAndFlushLog: unit -> unit
