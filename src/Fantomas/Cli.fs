module Fantomas.Cli

open System.IO.Abstractions
open Serilog
open Fantomas
open Fantomas.Core
open Fantomas.Logging
open Fantomas.Theme

[<NoComparison; NoEquality>]
type CliEnvironment =
    {
        FileSystem: IFileSystem
        IgnoreFile: IgnoreFile option
        ReadConfiguration: string -> FormatConfig
        Log: ILogger
        OutputTheme: Theme
        ErrorTheme: Theme
    }

type CliSettings =
    {
        Force: bool
        Verbosity: VerbosityLevel
    }
