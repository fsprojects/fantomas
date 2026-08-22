module Fantomas.CheckCommand

open Fantomas
open Fantomas.Arguments
open Fantomas.CommandResult

/// Format every file in memory and report which ones came out different, without writing anything.
val checkCode: filenames: string seq -> Async<CheckResult>

/// Find out which files need formatting, writing nothing. What was found is returned rather than
/// printed, so that a caller can inspect it before any of it reaches a console.
val runCheckCommand: ignoreFile: IgnoreFile option -> inputPath: InputPath -> CheckCommandResult
