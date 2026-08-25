module Fantomas.DaemonCommand

open System.IO.Abstractions
open Serilog

/// Serve editor tooling over JSON-RPC on standard in and standard out until the client closes the
/// connection, and return the exit code the process should end with.
///
/// Its own module, the way every other command has one, so that `main` reads as a choice between
/// commands rather than as one command written out in the middle of the others.
///
/// It takes a file system and a logger rather than a `CliEnvironment`, because it is told what to
/// format by the client rather than by a path on the command line: it has no input to plan, no
/// ignore file to resolve, and nothing to colour, and standard out belongs to the protocol.
val runDaemonCommand: fs: IFileSystem -> log: ILogger -> int
