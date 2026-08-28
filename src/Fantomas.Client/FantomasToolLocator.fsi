module Fantomas.Client.FantomasToolLocator

open Fantomas.Client.LSPFantomasServiceTypes

val findFantomasTool: workingDir: Folder -> Result<FantomasToolFound, FantomasToolError>

/// How this version of Fantomas wants its daemon asked for. `fantomas daemon` from 8.0, and
/// `fantomas --daemon` for everything before it, which every version understands and always will.
val daemonArgument: version: FantomasVersion -> string

/// Start the daemon, asking for it the way the version found spells it.
val createForVersion:
    version: FantomasVersion -> startInfo: FantomasToolStartInfo -> Result<RunningFantomasTool, ProcessStartError>

/// Start the daemon without knowing which version it is, which asks for it the way every version
/// understands. Prefer `createForVersion` where the version is in hand.
val createFor: startInfo: FantomasToolStartInfo -> Result<RunningFantomasTool, ProcessStartError>
