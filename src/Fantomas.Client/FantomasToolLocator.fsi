module Fantomas.Client.FantomasToolLocator

open Fantomas.Client.LSPFantomasServiceTypes

val findFantomasTool: workingDir: Folder -> Result<FantomasToolFound, FantomasToolError>

val createFor: startInfo: FantomasToolStartInfo -> Result<RunningFantomasTool, ProcessStartError>
