module Fantomas.Logging

[<RequireQualifiedAccess>]
type VerbosityLevel =
    | Normal
    | Detailed

val initLogger: level: VerbosityLevel -> VerbosityLevel

/// log a message
val stdlog: s: string -> unit

/// log an error
val elog: s: string -> unit

/// log a message if the verbosity level is >= Detailed
val logGrEqDetailed: s: string -> unit

val closeAndFlushLog: unit -> unit
