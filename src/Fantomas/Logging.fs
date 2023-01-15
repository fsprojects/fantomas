module Fantomas.Logging

open Serilog

[<RequireQualifiedAccess>]
type VerbosityLevel =
    | Normal
    | Detailed

let private logger =
    Log.Logger <- LoggerConfiguration().WriteTo.Console().CreateLogger()
    Log.Logger

/// log a message
let stdlog (s: string) = logger.Information(s)

/// log an error
let elog (s: string) = logger.Error(s)

/// log a message if the verbosity level is >= Detailed
let logGrEqDetailed verbosity s =
    if verbosity = VerbosityLevel.Detailed then
        logger.Information(s)
    else
        ()

let closeAndFlushLog () = Log.CloseAndFlush()
