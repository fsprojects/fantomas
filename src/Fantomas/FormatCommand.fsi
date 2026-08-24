module Fantomas.FormatCommand

open Fantomas.Core
open Fantomas.Arguments
open Fantomas.Cli
open Fantomas.CommandResult

type FormatParams =
    {
        Config: FormatConfig
        CompareWithoutLineEndings: bool
        Profile: bool
        File: string
    }

    static member Create: FormatConfig * bool * bool * string -> FormatParams

/// Format content that is already in hand. Whether the file should have been formatted at all is
/// the caller's business, decided once in `Plan`.
val formatContentAsync: formatParams: FormatParams -> originalContent: string -> Async<FormatResult>

/// Format the files the input path names, writing each result where the output path says. What
/// happened is returned rather than printed, so that a caller can inspect it before any of it
/// reaches a console.
val runFormatCommand:
    env: CliEnvironment ->
    settings: CliSettings ->
    inputPath: InputPath ->
    outputPath: OutputPath ->
        FormatCommandResult
