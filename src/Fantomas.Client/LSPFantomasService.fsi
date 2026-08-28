module Fantomas.Client.LSPFantomasService

open Fantomas.Client.LSPFantomasServiceTypes

/// Which daemon serves which folder. `'daemon` is anything satisfying `IDaemon`; the tool itself
/// uses a `RunningFantomasTool`, and a test uses a record with a flag on it.
[<NoComparison; NoEquality>]
type internal ServiceState<'daemon> =
    {
        Daemons: Map<FantomasVersion, 'daemon>
        FolderToVersion: Map<Folder, FantomasVersion>
    }

    static member Empty: ServiceState<'daemon>

[<RequireQualifiedAccess>]
type internal GetDaemonError =
    | DotNetToolListError of error: DotNetToolListError
    | FantomasProcessStart of error: ProcessStartError
    | InCompatibleVersionFound
    /// Something the cache does not model went wrong while resolving a daemon. Carried so that the
    /// caller gets an answer: the mailbox behind this is reached by a `PostAndReply` with no
    /// timeout, and a loop that died would leave every later request waiting forever.
    | UnexpectedException of error: string

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
    {
        FindTool: Folder -> Result<FantomasToolFound, FantomasToolError>
        Create: FantomasVersion -> FantomasToolStartInfo -> Result<'daemon, ProcessStartError>
    }

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

    /// Report what the service does that no `FantomasResponse` can carry: which Fantomas a folder
    /// resolved to and where it was found, when none could be found at all, and when a daemon is
    /// started or fails to start. Resolving to a tool on the PATH is the one a user most needs to
    /// see, since it formats exactly as successfully as the version their repository pins.
    ///
    /// Called on whichever thread the work happened on, which for tool resolution is the service's
    /// own mailbox rather than the caller's, and never more than once per folder for a resolution:
    /// a folder already mapped to a version does not resolve again. A delegate that throws is
    /// swallowed, because these are called from inside that mailbox and an exception out of there
    /// would leave every later request waiting on a reply that never comes.
    ///
    /// A delegate rather than a logging abstraction, so that this package keeps its three
    /// dependencies and a host can adapt it to whatever it already has.
    new: log: System.Action<FantomasLogLevel, string> -> LSPFantomasService

    /// Nothing is logged.
    new: unit -> LSPFantomasService
