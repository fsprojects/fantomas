module Fantomas.FormatCommand

open Fantomas.Core
open Fantomas.Arguments
open Fantomas.Cli
open Fantomas.CommandResult

/// What one file is to be formatted with, and how its result is to be compared with what it was.
type FormatParams =
    {
        Config: FormatConfig
        /// Whether a difference in line endings alone counts as a difference.
        ///
        /// `true` compares with the carriage returns stripped, so a file that differs from its
        /// formatted self only in how its lines end reads as already formatted. That is what
        /// `--check` wants: a working tree checked out with the other platform's line endings is
        /// not a tree that needs formatting, and reporting it as one would fail a pipeline over
        /// something the formatter never chose.
        ///
        /// `false` compares the text as it is, which is what a format run wants: the line endings
        /// are the formatter's to settle, so a file that differs only there is still rewritten.
        CompareWithoutLineEndings: bool
        File: string
    }

    static member Create: config: FormatConfig * compareWithoutLineEndings: bool * file: string -> FormatParams

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
