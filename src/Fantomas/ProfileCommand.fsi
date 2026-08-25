module Fantomas.ProfileCommand

open System
open Fantomas.Arguments
open Fantomas.Cli
open Fantomas.CommandResult

/// How long one file took, how big it was, and how many times it had to be formatted.
type FileTiming =
    {
        File: string
        LineCount: int
        /// How many combinations of defines the file was formatted for and merged from. One for a
        /// file with no conditional directives, which is nearly all of them, and what explains a
        /// short file costing more than a long one when it is not.
        DefineCombinations: int
        TimeTaken: TimeSpan
    }

/// What a profile run measured.
[<NoComparison>]
type ProfileResult =
    {
        /// Slowest first, which is the order the question is asked in.
        Timings: FileTiming list
        Ignored: string list
        Errors: (string * exn) list
        /// The whole run, which is not the sum of the timings: reading each file and walking the
        /// folder are in here and in none of them.
        Elapsed: TimeSpan
    }

/// What a profile run did.
[<RequireQualifiedAccess; NoComparison>]
type ProfileCommandResult =
    | InvalidInput of problem: InputProblem
    | Completed of result: ProfileResult
    /// Something was raised that no single file could be blamed for.
    | Failed of error: exn

    /// The exit code the process should end with: 1 when anything failed, 0 otherwise.
    member ExitCode: int

/// Format every file the input path names, in memory and one at a time, and report how long each
/// took. Nothing is written.
///
/// Serially on purpose. Formatting runs in parallel everywhere else, which means each file's
/// stopwatch measures wall clock under contention rather than the work on that file: a signature
/// file of six lines and a source file of several thousand came back with the same figure. A
/// diagnostic whose numbers cannot be compared is not one, and a run that takes longer is a price
/// worth paying for numbers that mean something.
val runProfileCommand: env: CliEnvironment -> inputPath: InputPath -> ProfileCommandResult
