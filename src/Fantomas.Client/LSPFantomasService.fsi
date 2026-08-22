module Fantomas.Client.LSPFantomasService

open Fantomas.Client.LSPFantomasServiceTypes

/// Which daemon serves which folder. `'daemon` is anything satisfying `IDaemon`; the tool itself
/// uses a `RunningFantomasTool`, and a test uses a record with a flag on it.
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

/// What the cache needs of a daemon it is holding: enough to tell a live one from a crashed one,
/// to start a replacement the way the original was started, and to let one go.
///
/// This, rather than `RunningFantomasTool`, is what `'daemon` stands for everywhere in this module.
/// It is an interface so that the cache can be exercised without starting a process: everything
/// here is a question about a daemon, and none of it needs the daemon to be a real one.
type internal IDaemon =
    inherit System.IDisposable

    /// How this daemon was started, so a crashed one can be replaced the way it was created rather
    /// than the way the folder asking for it resolves now.
    abstract StartInfo: FantomasToolStartInfo

    /// Whether it can still serve a request: the process is up and the connection to it has not
    /// ended. A daemon failing either half is disposed and replaced.
    abstract IsRunning: bool

/// How the cache reaches the world. Only the two things it cannot answer for itself: everything a
/// daemon knows about itself is on `IDaemon`.
[<NoComparison; NoEquality>]
type internal DaemonOperations<'daemon when 'daemon :> IDaemon> =
    { FindTool: Folder -> Result<FantomasToolFound, FantomasToolError>
      Create: FantomasToolStartInfo -> Result<'daemon, ProcessStartError> }

/// Hand out the daemon serving `folder`, starting one if no running daemon serves its version yet,
/// along with the cache that leaves behind. Daemons are keyed by version rather than by folder, so
/// two folders that pin the same Fantomas share one process.
///
/// Never leaves a folder pinned to a version with no daemon behind it, and recovers rather than
/// erroring if it is ever handed a cache in that state.
val internal resolveDaemon:
    operations: DaemonOperations<'daemon> ->
    state: ServiceState<'daemon> ->
    folder: Folder ->
        Result<'daemon, GetDaemonError> * ServiceState<'daemon>
        when 'daemon :> IDaemon

type LSPFantomasService =
    interface Contracts.FantomasService

    new: unit -> LSPFantomasService
