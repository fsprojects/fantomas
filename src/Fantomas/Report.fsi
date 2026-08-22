module Fantomas.Report

open Fantomas.Cli
open Fantomas.CommandResult

/// The single wording for each way the input paths can fail to make sense. Both commands are
/// described from here, so neither can drift away from the other.
val describeInputProblem: problem: InputProblem -> string

/// Report what a format run did, and return the exit code the process should end with: 1 when
/// anything failed, 0 otherwise.
val reportFormatCommand: env: CliEnvironment -> settings: CliSettings -> result: FormatCommandResult -> int

/// Report what a `--check` run found, and return the exit code the process should end with: 0 when
/// every file is already formatted, 99 when at least one needs formatting, and 1 on failure.
val reportCheckCommand: env: CliEnvironment -> result: CheckCommandResult -> int
