module Fantomas.Cli

open System.IO.Abstractions
open Serilog
open Spectre.Console
open Fantomas
open Fantomas.Core
open Fantomas.Logging

[<NoComparison; NoEquality>]
type CliEnvironment =
    {
        FileSystem: IFileSystem
        IgnoreFile: IgnoreFile option
        ReadConfiguration: string -> FormatConfig
        Log: ILogger
        Console: IAnsiConsole
    }

type CliSettings =
    {
        Force: bool
        Profile: bool
        Verbosity: VerbosityLevel
    }
