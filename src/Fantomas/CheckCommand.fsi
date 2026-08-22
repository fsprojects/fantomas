module Fantomas.CheckCommand

open Fantomas.Arguments
open Fantomas.Cli
open Fantomas.CommandResult

/// Format every file in memory and report which ones came out different, without writing anything.
val checkCode: env: CliEnvironment -> filenames: string seq -> Async<CheckResult>

/// Find out which files need formatting, writing nothing. What was found is returned rather than
/// printed, so that a caller can inspect it before any of it reaches a console.
val runCheckCommand: env: CliEnvironment -> inputPath: InputPath -> CheckCommandResult
