module Fantomas.Cli

open System.IO.Abstractions
open Serilog
open Fantomas
open Fantomas.Core
open Fantomas.Logging
open Fantomas.Theme

/// How the tool reaches the world outside itself. Built once, in `main`, and handed down. A test
/// builds one over a `MockFileSystem` and a configuration of its choosing, and so never needs the
/// real file system or the directory the test host happens to be running in.
[<NoComparison; NoEquality>]
type CliEnvironment =
    {
        FileSystem: IFileSystem
        /// The `.fantomasignore` that governs a file: the nearest one at or above it. A function
        /// rather than one found up front, because which one applies depends on the file, and
        /// resolving it once for the whole run is what made a pipeline disagree with an editor
        /// about a `.fantomasignore` in a subfolder.
        FindIgnoreFile: string -> IgnoreFile option
        /// The `.fantomasignore` files above the one that governs a file, nearest first, none of
        /// which applies. Beside `FindIgnoreFile` rather than folded into it for the reason
        /// `ResolveConfiguration` sits beside `ReadConfiguration`: it costs a second walk and only
        /// `doctor` has anything to do with the answer.
        FindIgnoreFilesAbove: IgnoreFile -> IgnoreFile list
        /// The `.editorconfig` settings for a file. A function rather than a file system read,
        /// because the editorconfig parser reads the disk itself and cannot be given one.
        ReadConfiguration: string -> FormatConfig
        /// The same settings taken apart: each with the `.editorconfig` that set it, or nothing
        /// where the Fantomas default is what applies. Beside `ReadConfiguration` rather than in
        /// place of it, because working out where a value came from costs a second walk of the
        /// chain and only `doctor` has anything to do with the answer.
        ResolveConfiguration: string -> EditorConfig.ResolvedConfig
        /// Where the tool writes. A test hands over a logger with a collecting sink instead of
        /// reading a console. Which stream a level lands on is the logger's own configuration,
        /// not this record's.
        Log: ILogger
        /// What standard out will take. Held rather than detected at the point of writing, so a
        /// test pins it and asserts on plain text.
        OutputTheme: Theme
        /// What standard error will take. Separate because the two streams are redirected
        /// separately, and the messages that go to each are decided by level.
        ErrorTheme: Theme
        /// How Fantomas was started, spelled the way the reader would type it again, so that a
        /// message suggesting a command suggests one they have. Held rather than asked of the
        /// process at the point of writing, for the reason the themes are: reaching for
        /// `Environment.ProcessPath` mid-render made what a reporter printed depend on what
        /// happened to be running it, and under a test host that is `testhost` on one platform and
        /// `dotnet` on another.
        Invocation: string
    }

/// What the user asked for. Kept apart from the environment because a test varies these on every
/// case and the environment on almost none.
type CliSettings =
    {
        Force: bool
        Verbosity: VerbosityLevel
    }
