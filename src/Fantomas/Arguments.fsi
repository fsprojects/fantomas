module Fantomas.Arguments

open System.IO.Abstractions
open Fantomas.Logging

/// One thing the command line asked for. Repeating a flag is allowed and the last one wins, so a
/// parsed command line holds at most one of each.
///
/// Not `RequireQualifiedAccess`: the module is called `Arguments` too, so `Arguments.Check` would
/// be read as a path through the module rather than as the type's case.
type Arguments =
    | Force
    | Profile
    | Out of string
    | Check
    | Json
    | Daemon
    | Version
    | Help
    | Verbosity of string
    | Input of string list

/// A reason the command line could not be read. Every one of these names the flag it is about, so
/// the message can quote back what was typed rather than describe it.
[<RequireQualifiedAccess>]
type ArgumentProblem =
    /// A token beginning with a dash that is not a flag Fantomas has. The suggestion is the nearest
    /// flag it could be a misspelling of, when there is one close enough to be help rather than
    /// noise.
    | UnknownFlag of flag: string * suggestion: string option
    /// A flag that takes a value, with nothing usable after it. A token beginning with a dash is
    /// never taken as a value, so `--out --check` is this rather than an output path of `--check`.
    | MissingValue of flag: string * found: string option
    /// `--check=true`, where the flag is a switch and takes no value.
    | UnexpectedValue of flag: string * value: string
    /// A flag whose value has to be one of a few words, given something else.
    | UnreadableValue of flag: string * value: string * accepted: string list
    /// Arguments that mean nothing beside `--daemon`, which used to be accepted and then ignored.
    | RefusedWithDaemon of refused: string list

/// Read the command line.
///
/// Flags and paths interleave freely and in any order, so `fantomas src --check tests` and
/// `fantomas --check src tests` are the same command. A flag that takes a value accepts it as the
/// next token or attached with `=`. `--` ends the flags, and everything after it is a path. A
/// token beginning with a dash that is not a known flag is reported as one rather than mistaken
/// for a file that is not there.
val parse: argv: string array -> Result<Arguments list, ArgumentProblem>

/// What a problem reads as, in one sentence, quoting what was typed.
val describeArgumentProblem: problem: ArgumentProblem -> string

/// What the input paths on the command line were found to name.
[<RequireQualifiedAccess>]
type InputPath =
    | File of string
    | Folder of string
    | Multiple of files: string list * folder: string list
    | NoFSharpFile of string
    | NotFound of string
    | Unspecified

/// Where the result should be written, which is back over the input unless `--out` said otherwise.
[<RequireQualifiedAccess; Struct>]
type OutputPath =
    | IO of string
    | NotKnown

/// Decide what the paths on the command line name, by asking the file system. Several paths are
/// all required to exist before any of them is classified, so one path that is not there is
/// reported rather than the rest being worked on.
val classifyInputPath: fs: IFileSystem -> maybeInput: string list option -> InputPath

/// Read a `--verbosity` value that was given. `None` means it was not one Fantomas knows.
///
/// Only a value that is there. What a run with no `--verbosity` at all should do is a default, and
/// defaulting is the caller's to decide rather than something to fold into a parse: taking an
/// option here made "not asked for" and "asked for correctly" the same answer, so the one caller
/// that has to tell them apart could not.
val parseVerbosity: value: string -> VerbosityLevel option

/// How an argument is spelled on the command line, for a message that has to name one back.
val describeArgument: argument: Arguments -> string

/// The value of the flag, when the command line carried it. One accessor each rather than a
/// general one, because a caller that has to write the pattern out is a caller that can get it
/// subtly wrong, and there are only three values to reach for.
val tryOut: given: Arguments list -> string option

val tryVerbosity: given: Arguments list -> string option

val tryInput: given: Arguments list -> string list option

/// The input paths as the caller gave them, spelled so that a message can suggest a command the
/// caller can run again. Several paths are joined by a space, which is how they were typed.
val describeInputPaths: inputPath: InputPath -> string

/// The arguments given alongside `--daemon` that mean nothing there, spelled as they are typed and
/// in a settled order, so a run that names several always names them the same way. Empty when
/// `--daemon` was not asked for, so a caller does not have to check that first.
///
/// A daemon is told what to format over JSON-RPC and answers on standard out, so nothing that says
/// what to format, where to put it, or how to report it has anything to apply to. Refusing them is
/// the point: every one of these used to be accepted and then silently ignored.
///
/// Two are not refused. `--verbosity` sets the level the daemon logs at, so it is the one argument
/// here that does something. `--version` is answered and exited on before this rule is ever asked,
/// so it wins rather than being refused.
val argumentsRefusedWithDaemon: given: Arguments list -> string list
