module Fantomas.Invocation

/// How Fantomas was started, spelled the way the reader would type it again.
///
/// A global tool is an apphost on the path, so the process is `fantomas` itself. A local tool is
/// started by the dotnet muxer, so the process is `dotnet` and the command is `dotnet fantomas`.
/// Anything else is named back as it was found, which covers a `--tool-path` install and a renamed
/// apphost.
val nameOf: processPath: string option -> string

/// `nameOf` for the running process.
val name: unit -> string
