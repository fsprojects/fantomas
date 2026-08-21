module Fantomas.Logging

open System
open Serilog
open Serilog.Events

[<RequireQualifiedAccess>]
type VerbosityLevel =
    | Normal
    | Detailed

let createLogger (level: VerbosityLevel) (standardErrorFromLevel: LogEventLevel) : VerbosityLevel =
    let configuration =
        match level with
        | VerbosityLevel.Normal ->
            LoggerConfiguration()
                .MinimumLevel.Information()
                .WriteTo.Console(
                    outputTemplate = "{Message:lj}{NewLine}{Exception}",
                    standardErrorFromLevel = Nullable standardErrorFromLevel
                )
        | VerbosityLevel.Detailed ->
            LoggerConfiguration()
                .MinimumLevel.Debug()
                .WriteTo.Console(standardErrorFromLevel = Nullable standardErrorFromLevel)

    Log.Logger <- configuration.CreateLogger()
    level

let initLogger (level: VerbosityLevel) : VerbosityLevel =
    createLogger level LogEventLevel.Warning

let initDaemonLogger (level: VerbosityLevel) : VerbosityLevel =
    createLogger level LogEventLevel.Verbose

let stdlog (s: string) = Log.Logger.Information(s)

let elog (s: string) = Log.Logger.Error(s)

let logGrEqDetailed s = Log.Logger.Debug(s)

let closeAndFlushLog () = Log.CloseAndFlush()
