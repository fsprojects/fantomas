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

/// log a message
let stdlog (s: string) = Log.Logger.Information(s)

/// log an error
let elog (s: string) = Log.Logger.Error(s)

/// log a message if the verbosity level is >= Detailed
let logGrEqDetailed s = Log.Logger.Debug(s)

let closeAndFlushLog () = Log.CloseAndFlush()
