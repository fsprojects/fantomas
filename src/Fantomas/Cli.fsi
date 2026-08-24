module Fantomas.Cli

open System.IO.Abstractions
open Serilog
open Spectre.Console
open Fantomas
open Fantomas.Core
open Fantomas.Logging

/// How the tool reaches the world outside itself. Built once, in `main`, and handed down. A test
/// builds one over a `MockFileSystem` and a configuration of its choosing, and so never needs the
/// real file system or the directory the test host happens to be running in.
[<NoComparison; NoEquality>]
type CliEnvironment =
    {
        FileSystem: IFileSystem
        /// The single `.fantomasignore` this run honours, already found.
        IgnoreFile: IgnoreFile option
        /// The `.editorconfig` settings for a file. A function rather than a file system read,
        /// because the editorconfig parser reads the disk itself and cannot be given one.
        ReadConfiguration: string -> FormatConfig
        /// Where the tool writes. A test hands over a logger with a collecting sink instead of
        /// reading a console. Which stream a level lands on is the logger's own configuration,
        /// not this record's.
        Log: ILogger
        /// Spectre's console, for the one thing drawn rather than written: the summary table.
        Console: IAnsiConsole
    }

/// What the user asked for. Kept apart from the environment because a test varies these on every
/// case and the environment on almost none.
type CliSettings =
    {
        Force: bool
        Profile: bool
        Verbosity: VerbosityLevel
    }
