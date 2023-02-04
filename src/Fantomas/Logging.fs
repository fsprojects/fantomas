module Fantomas.Logging

open Serilog

[<RequireQualifiedAccess>]
type VerbosityLevel =
    | Normal
    | Detailed

let initLogger (level: VerbosityLevel) : VerbosityLevel =
    let logger =
        match level with
        | VerbosityLevel.Normal ->
            LoggerConfiguration()
                .MinimumLevel.Information()
                .WriteTo.Console(outputTemplate = "{Message:lj}{NewLine}{Exception}")
                .CreateLogger()
        | VerbosityLevel.Detailed -> LoggerConfiguration().MinimumLevel.Debug().WriteTo.Console().CreateLogger()

    Log.Logger <- logger
    level

let logger = Log.Logger

/// log a message
let stdlog (s: string) = logger.Information(s)

/// log an error
let elog (s: string) = logger.Error(s)

/// log a message if the verbosity level is >= Detailed
let logGrEqDetailed s = logger.Debug(s)

let closeAndFlushLog () = Log.CloseAndFlush()
