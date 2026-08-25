module Fantomas.Logging

open System
open Serilog
open Serilog.Events
open Serilog.Sinks.SystemConsole.Themes

[<RequireQualifiedAccess; Struct>]
type VerbosityLevel =
    | Normal
    | Detailed

// The sink colours what it writes unless it is told not to, and its default theme paints the
// message bright white. Fantomas decides its own colour now, glyph by glyph and word by word, and
// the two cannot both be in charge: bright white overrides the terminal's own foreground, which on
// a light background is the one colour that cannot be read. So the sink writes what it is given and
// nothing more.
let createLogger (level: VerbosityLevel) (standardErrorFromLevel: LogEventLevel) : VerbosityLevel =
    let configuration =
        match level with
        | VerbosityLevel.Normal ->
            LoggerConfiguration()
                .MinimumLevel.Information()
                .WriteTo.Console(
                    outputTemplate = "{Message:lj}{NewLine}{Exception}",
                    standardErrorFromLevel = Nullable standardErrorFromLevel,
                    theme = ConsoleTheme.None
                )
        | VerbosityLevel.Detailed ->
            LoggerConfiguration()
                .MinimumLevel.Debug()
                .WriteTo.Console(standardErrorFromLevel = Nullable standardErrorFromLevel, theme = ConsoleTheme.None)

    Log.Logger <- configuration.CreateLogger()
    level

let initLogger (level: VerbosityLevel) : VerbosityLevel =
    createLogger level LogEventLevel.Warning

let initDaemonLogger (level: VerbosityLevel) : VerbosityLevel =
    createLogger level LogEventLevel.Verbose

let initJsonLogger (level: VerbosityLevel) : VerbosityLevel =
    createLogger level LogEventLevel.Verbose

let closeAndFlushLog () : unit = Log.CloseAndFlush()
