module Fantomas.Client.FantomasToolLocator

open Fantomas.Client.LSPFantomasServiceTypes

/// The version `fantomas --version` printed, as `dotnet tool list` would have written it: no
/// `Fantomas` in front, no leading `v`, no `+<commit>` on the end, and folded to lower case as the
/// manifest side is. Daemons are cached by this string, so the same Fantomas resolved once from a
/// tool manifest and once from the PATH has to come out of both as the same text, or it gets two
/// processes.
val internal normalizeVersion: printed: string -> string

val findFantomasTool: workingDir: Folder -> Result<FantomasToolFound, FantomasToolError>

val createFor: startInfo: FantomasToolStartInfo -> Result<RunningFantomasTool, ProcessStartError>
