module Fantomas.Client.FantomasToolLocator

open Fantomas.Client.LSPFantomasServiceTypes

/// The version `fantomas --version` printed, as `dotnet tool list` would have written it: no
/// `Fantomas` in front, no leading `v`, no `+<commit>` on the end, and folded to lower case as the
/// manifest side is. Daemons are cached by this string, so the same Fantomas resolved once from a
/// tool manifest and once from the PATH has to come out of both as the same text, or it gets two
/// processes.
///
/// `--version` answers `Fantomas v8.0.0-alpha-014+e4a1c9d...`, `dotnet tool list` answers
/// `8.0.0-alpha-014`, and both have to arrive here as the latter.
val internal normalizeVersion: printed: string -> string

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
