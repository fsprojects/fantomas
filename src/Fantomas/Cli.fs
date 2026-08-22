module Fantomas.Cli

open System.IO.Abstractions
open Fantomas
open Fantomas.Core
open Fantomas.Logging

[<NoComparison; NoEquality>]
type CliEnvironment =
    { FileSystem: IFileSystem
      IgnoreFile: IgnoreFile option
      ReadConfiguration: string -> FormatConfig }

type CliSettings =
    { Force: bool
      Profile: bool
      Verbosity: VerbosityLevel }
