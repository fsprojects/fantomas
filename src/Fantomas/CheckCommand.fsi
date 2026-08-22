module Fantomas.CheckCommand

open Fantomas.Arguments
open Fantomas.CommandResult

/// Find out which files need formatting, writing nothing. What was found is returned rather than
/// printed, so that a caller can inspect it before any of it reaches a console.
val runCheckCommand: inputPath: InputPath -> CheckCommandResult
