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
        FindIgnoreFile: string -> IgnoreFile option
        FindIgnoreFilesAbove: IgnoreFile -> IgnoreFile list
        ReadConfiguration: string -> FormatConfig
        ResolveConfiguration: string -> EditorConfig.ResolvedConfig
        Log: ILogger
        OutputTheme: Theme
        ErrorTheme: Theme
        Invocation: string
    }

type CliSettings =
    {
        Force: bool
        Verbosity: VerbosityLevel
    }
