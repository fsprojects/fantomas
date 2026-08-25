module Fantomas.Arguments

open System.IO.Abstractions
open Argu
open Fantomas.Logging

[<HelpFlags("--help", "-h")>]
type Arguments =
    | [<Unique>] Force
    | [<Unique>] Profile
    | [<Unique>] Out of string
    | [<Unique>] Check
    | [<Unique>] Json
    | [<Unique>] Daemon
    | [<Unique>] Version
    | [<Unique; AltCommandLine("-v")>] Verbosity of string
    | [<MainCommand>] Input of string list

    interface IArgParserTemplate

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

/// Read the `--verbosity` value. `None` means the value was not one Fantomas knows.
val parseVerbosity: value: string option -> VerbosityLevel option

/// How an argument is spelled on the command line, for a message that has to name one back.
val describeArgument: argument: Arguments -> string

/// The input paths as the caller gave them, spelled so that a message can suggest a command the
/// caller can run again. Several paths are joined by a space, which is how they were typed.
val describeInputPaths: inputPath: InputPath -> string

/// The arguments given alongside `--daemon` that mean nothing there, spelled as they are typed and
/// in a settled order, so a run that names several always names them the same way.
///
/// A daemon is told what to format over JSON-RPC and answers on standard out, so nothing that says
/// what to format, where to put it, or how to report it has anything to apply to. Refusing them is
/// the point: every one of these used to be accepted and then silently ignored.
///
/// Two are not refused. `--verbosity` sets the level the daemon logs at, so it is the one argument
/// here that does something. `--version` is answered and exited on before this rule is ever asked,
/// so it wins rather than being refused.
val argumentsRefusedWithDaemon: given: Arguments list -> string list
