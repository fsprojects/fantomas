module Fantomas.Client.LSPFantomasService

open Fantomas.Client.LSPFantomasServiceTypes

/// Which daemon serves which folder. Written over an abstract daemon rather than over
/// `RunningFantomasTool` so that the cache can be exercised without starting a process: nothing
/// here does IO, every effect goes through `DaemonOperations`.
[<NoComparison; NoEquality>]
type internal ServiceState<'daemon> =
    { Daemons: Map<FantomasVersion, 'daemon>
      FolderToVersion: Map<Folder, FantomasVersion> }

    static member Empty: ServiceState<'daemon>

[<RequireQualifiedAccess>]
type internal GetDaemonError =
    | DotNetToolListError of error: DotNetToolListError
    | FantomasProcessStart of error: ProcessStartError
    | InCompatibleVersionFound
    /// A folder resolved to a version with no daemon behind it. `resolveDaemon` never leaves the
    /// cache in that state; reaching it means something else emptied `Daemons`.
    | CompatibleVersionIsKnownButNoDaemonIsRunning of version: FantomasVersion

/// Everything the cache does to a daemon and to the world, so that a test can supply all of it.
[<NoComparison; NoEquality>]
type internal DaemonOperations<'daemon> =
    { FindTool: Folder -> Result<FantomasToolFound, FantomasToolError>
      Create: FantomasToolStartInfo -> Result<'daemon, ProcessStartError>
      StartInfo: 'daemon -> FantomasToolStartInfo
      IsRunning: 'daemon -> bool
      Dispose: 'daemon -> unit }

/// Hand out the daemon serving `folder`, starting one if no running daemon serves its version yet,
/// along with the cache that leaves behind. Daemons are keyed by version rather than by folder, so
/// two folders that pin the same Fantomas share one process.
val internal resolveDaemon:
    operations: DaemonOperations<'daemon> ->
    state: ServiceState<'daemon> ->
    folder: Folder ->
        Result<'daemon, GetDaemonError> * ServiceState<'daemon>

type LSPFantomasService =
    interface Contracts.FantomasService

    new: unit -> LSPFantomasService
