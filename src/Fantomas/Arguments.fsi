module Fantomas.Arguments

open Argu
open Fantomas.Logging

[<HelpFlags("--help", "-h")>]
type Arguments =
    | [<Unique>] Force
    | [<Unique>] Profile
    | [<Unique>] Out of string
    | [<Unique>] Check
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
[<RequireQualifiedAccess>]
type OutputPath =
    | IO of string
    | NotKnown

/// Decide what the paths on the command line name. A single path is classified by asking the
/// file system about it. Several paths are all required to exist first, and are then told apart
/// by whether they carry a file extension, so `src` is taken as a folder and `src.fs` as a file.
val classifyInputPath: maybeInput: string list option -> InputPath

/// Read the `--verbosity` value. `None` means the value was not one Fantomas knows.
val parseVerbosity: value: string option -> VerbosityLevel option
