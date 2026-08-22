module FantomasClientTests.DaemonCacheTests

open NUnit.Framework
open Fantomas.Client.LSPFantomasService
open Fantomas.Client.LSPFantomasServiceTypes

/// A stand-in for a running daemon: everything `IDaemon` asks for and nothing else. A real
/// `RunningFantomasTool` needs a live process, and the questions here are all about which daemon
/// the cache hands out and what it forgets, not about processes.
type private FakeDaemon(id: int, startInfo: FantomasToolStartInfo) =
    member val Running = true with get, set
    member val Disposed = false with get, set
    member _.Id: int = id

    interface IDaemon with
        member _.StartInfo = startInfo
        member this.IsRunning = this.Running

        member this.Dispose() =
            this.Disposed <- true
            this.Running <- false

type private Recorder() =
    let created = ResizeArray<FakeDaemon>()

    member _.Created: FakeDaemon list = List.ofSeq created
    member _.Record(daemon: FakeDaemon) = created.Add daemon

let private version = FantomasVersion "8.0.0"
let private otherVersion = FantomasVersion "7.0.5"
let private folder = Folder "/repo"
let private nested = Folder "/repo/nested"

let private operationsFor
    (recorder: Recorder)
    (find: Folder -> Result<FantomasToolFound, FantomasToolError>)
    (create: FantomasToolStartInfo -> Result<FakeDaemon, ProcessStartError>)
    : DaemonOperations<FakeDaemon> =
    { FindTool = find
      Create =
        fun startInfo ->
            let created = create startInfo
            created |> Result.iter recorder.Record
            created }

let private findsVersion (version: FantomasVersion) (folder: Folder) =
    Ok(FantomasToolFound(version, FantomasToolStartInfo.LocalTool folder))

let private startsFine (nextId: int ref) (startInfo: FantomasToolStartInfo) =
    nextId.Value <- nextId.Value + 1
    Ok(FakeDaemon(nextId.Value, startInfo))

let private neverStarts (_: FantomasToolStartInfo) =
    Error(ProcessStartError.UnExpectedException("dotnet", "fantomas --daemon", "boom"))

let private expectOk (result: Result<'a, GetDaemonError>) : 'a =
    match result with
    | Ok value -> value
    | Error error ->
        Assert.Fail $"Expected a daemon, got %A{error}"
        failwith "unreachable"

[<Test>]
let ``two folders on the same version share one daemon`` () =
    let recorder = Recorder()
    let operations = operationsFor recorder (findsVersion version) (startsFine (ref 0))

    let first, state = resolveDaemon operations ServiceState.Empty folder
    let second, state = resolveDaemon operations state nested

    (expectOk first).Id |> ignore
    Assert.That((expectOk second).Id, Is.EqualTo (expectOk first).Id)
    Assert.That(List.length recorder.Created, Is.EqualTo 1)

    // Both folders resolved, and the one daemon is the one they resolved to.
    Assert.That(Map.count state.FolderToVersion, Is.EqualTo 2)
    Assert.That(Map.count state.Daemons, Is.EqualTo 1)

[<Test>]
let ``a folder already resolved is served from the cache without looking for a tool again`` () =
    let recorder = Recorder()
    let looked = ref 0

    let find folder =
        looked.Value <- looked.Value + 1
        findsVersion version folder

    let operations = operationsFor recorder find (startsFine (ref 0))

    let _, state = resolveDaemon operations ServiceState.Empty folder
    let _, _ = resolveDaemon operations state folder

    Assert.That(looked.Value, Is.EqualTo 1)

[<Test>]
let ``a daemon that crashed is replaced, and the one it replaces is disposed`` () =
    let recorder = Recorder()
    let operations = operationsFor recorder (findsVersion version) (startsFine (ref 0))

    let first, state = resolveDaemon operations ServiceState.Empty folder
    let first = expectOk first
    first.Running <- false

    let second, _ = resolveDaemon operations state folder
    let second = expectOk second

    Assert.That(second.Id, Is.Not.EqualTo first.Id)
    Assert.That(first.Disposed, Is.True)
    // Restarted the way the daemon it replaces was started, not the way this folder resolves now.
    Assert.That((second :> IDaemon).StartInfo, Is.EqualTo (first :> IDaemon).StartInfo)

// The regression this is here for: dropping a version from the cache used to leave every folder
// that resolved to it pointing at a version with no daemon. That is the one state resolveDaemon
// cannot get out of, so the folder answered CompatibleVersionIsKnownButNoDaemonIsRunning for the
// rest of the session instead of trying again.
[<Test>]
let ``a folder recovers after the daemon for its version failed to start`` () =
    let recorder = Recorder()
    let nextId = ref 0
    let working = ref false

    let create startInfo =
        if working.Value then
            startsFine nextId startInfo
        else
            neverStarts startInfo

    let operations = operationsFor recorder (findsVersion version) create

    let failed, state = resolveDaemon operations ServiceState.Empty folder

    match failed with
    | Error(GetDaemonError.FantomasProcessStart _) -> ()
    | otherwise -> Assert.Fail $"Expected a start failure, got %A{otherwise}"

    // Nothing left behind pointing at a version with no daemon.
    Assert.That(state.FolderToVersion, Is.Empty)
    Assert.That(state.Daemons, Is.Empty)

    working.Value <- true
    let recovered, _ = resolveDaemon operations state folder
    expectOk recovered |> ignore

[<Test>]
let ``a second folder recovers too when the daemon they share failed to start`` () =
    let recorder = Recorder()
    let nextId = ref 0
    let working = ref true

    let create startInfo =
        if working.Value then
            startsFine nextId startInfo
        else
            neverStarts startInfo

    let operations = operationsFor recorder (findsVersion version) create

    // Both folders resolve to the one daemon, then it crashes and will not come back.
    let first, state = resolveDaemon operations ServiceState.Empty folder
    let _, state = resolveDaemon operations state nested
    (expectOk first).Running <- false
    working.Value <- false

    let _, state = resolveDaemon operations state folder

    // The other folder shared that version, so it must not be left pinned to it either.
    Assert.That(state.FolderToVersion, Is.Empty)

    working.Value <- true
    let recovered, _ = resolveDaemon operations state nested
    expectOk recovered |> ignore

[<Test>]
let ``forgetting one version leaves the folders on another version alone`` () =
    let recorder = Recorder()
    let nextId = ref 0
    let working = ref true

    let create startInfo =
        if working.Value then
            startsFine nextId startInfo
        else
            neverStarts startInfo

    let find (asked: Folder) =
        if asked = nested then
            findsVersion otherVersion asked
        else
            findsVersion version asked

    let operations = operationsFor recorder find create

    let _, state = resolveDaemon operations ServiceState.Empty folder
    let survivor, state = resolveDaemon operations state nested
    let survivor = expectOk survivor

    // The daemon for `folder`'s version dies and cannot be restarted.
    (state.Daemons |> Map.find version).Running <- false
    working.Value <- false
    let _, state = resolveDaemon operations state folder

    let remaining: (Folder * FantomasVersion) list = Map.toList state.FolderToVersion
    Assert.That(remaining, Is.EqualTo<(Folder * FantomasVersion) list> [ nested, otherVersion ])
    Assert.That(state.Daemons |> Map.find otherVersion, Is.SameAs survivor)

[<Test>]
let ``no compatible version is reported without touching the cache`` () =
    let recorder = Recorder()

    let operations =
        operationsFor recorder (fun _ -> Error FantomasToolError.NoCompatibleVersionFound) (startsFine (ref 0))

    let result, state = resolveDaemon operations ServiceState.Empty folder

    match result with
    | Error GetDaemonError.InCompatibleVersionFound -> ()
    | otherwise -> Assert.Fail $"Expected no compatible version, got %A{otherwise}"

    Assert.That(state.FolderToVersion, Is.Empty)
    Assert.That(recorder.Created, Is.Empty)
