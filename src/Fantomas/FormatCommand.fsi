module Fantomas.FormatCommand

open Fantomas.Arguments
open Fantomas.CommandResult

/// Format the files the input path names, writing each result where the output path says. What
/// happened is returned rather than printed, so that a caller can inspect it before any of it
/// reaches a console.
val runFormatCommand:
    force: bool -> profile: bool -> inputPath: InputPath -> outputPath: OutputPath -> FormatCommandResult
