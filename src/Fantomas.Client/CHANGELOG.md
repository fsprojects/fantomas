# Changelog

This is the changelog for the Fantomas.Client package specifically. It's distinct from that of the overall libraries and command-line tool.

## [0.12.0-beta-001] - 2026-08-28

### Added
- `FantomasToolLocator.createForVersion` and `FantomasToolLocator.daemonArgument`. Fantomas 8.0 added `fantomas daemon` beside `fantomas --daemon`, and a client that knows which version it found now asks for the daemon the way that version spells it. The command arrived in `8.0.0-alpha-016`, so that release and everything after it is asked by command and everything before it by flag, prereleases included; a version string that cannot be parsed falls back to the flag. `createFor` is unchanged, still asks with the flag, and still works against every Fantomas there has ever been, so a caller that has no version in hand needs to change nothing. [#3416](https://github.com/fsprojects/fantomas/pull/3416)
- `FantomasService.ConfigurationWarnings`, an event raised for every format request with the settings in the resolved configuration that Fantomas could not act on: a `fsharp_`-prefixed setting the daemon's version does not have, or a setting carrying a value it cannot parse. Formatting still succeeds using defaults, so without this the setting quietly does not apply. The event is raised with an empty `Problems` array when nothing is wrong, so a subscriber can clear what it reported earlier, and it is raised on the thread the daemon's message arrived on rather than on the caller's. Only Fantomas 8 daemons send these; against an older daemon the event never fires, so no version check is needed. See [Formatting from an editor with Fantomas.Client](https://fsprojects.github.io/fantomas/docs/end-users/FantomasClient.html). [#3401](https://github.com/fsprojects/fantomas/pull/3401)
- `ConfigurationWarning`, `ConfigurationProblem`, `ConfigurationProblemCode` and `ConfigurationProblemSource`, the types carried by that event, and `Methods.ConfigurationWarning`, the JSON-RPC method they arrive on. [#3401](https://github.com/fsprojects/fantomas/pull/3401)
- `RunningFantomasTool.ConfigurationWarnings`, for callers driving `FantomasToolLocator.createFor` themselves rather than going through `LSPFantomasService`. The handler has to be registered before the connection starts listening, so `createFor` wires it up; its own signature is unchanged. [#3401](https://github.com/fsprojects/fantomas/pull/3401)
- `LSPFantomasService(log)`, a constructor overload taking an `Action<FantomasLogLevel, string>`, and the `FantomasLogLevel` it reports at. It says which Fantomas a folder resolved to and where it was found, when none could be found at all, and when a daemon is started or fails to start. None of that fits in a `FantomasResponse`, and a tool resolved from the `PATH` formats exactly as successfully as the version a repository pins, so until now nothing told those two apart. A delegate rather than a logging abstraction, so this package keeps its three dependencies and a host adapts it to whatever it already has. The parameterless constructor is unchanged and logs nothing. [#3401](https://github.com/fsprojects/fantomas/pull/3401)

### Changed
- Bump `StreamJsonRpc` to `2.25.29`. [#3393](https://github.com/fsprojects/fantomas/pull/3393)
- Breaking: no longer binary compatible with `0.11.0`. `FantomasService` gained a member, `RunningFantomasTool` gained a field, and several discriminated unions are structs now. None of that changes how any of them is written or matched, but an assembly compiled against `0.11.0` has to be rebuilt. [#3401](https://github.com/fsprojects/fantomas/pull/3401) [#3407](https://github.com/fsprojects/fantomas/pull/3407)
- Breaking: `FantomasService` gained the `ConfigurationWarnings` member, and `RunningFantomasTool` gained a field. Source-breaking only for code that *implements* `FantomasService` or *constructs* `RunningFantomasTool`; code that only calls either of them needs no source change. [#3401](https://github.com/fsprojects/fantomas/pull/3401)
- The `DaemonCreationFailed` response no longer reports "found a compatible version but no daemon could be launched". The cache state behind that message cannot arise now that a version is forgotten along with the folders pinned to it, and a cache handed that state resolves the tool again rather than reporting it. [#3401](https://github.com/fsprojects/fantomas/pull/3401)
- Breaking: `Methods` carries `RequireQualifiedAccess` now, so `open Fantomas.Client.Contracts.Methods` no longer compiles and the literals are reached as `Methods.FormatDocument`. Source-breaking only for code that opened the module; a call site that already qualified is unaffected. [#3402](https://github.com/fsprojects/fantomas/pull/3402)
- Breaking: `FantomasVersion` carries an internal case now, so it can no longer be constructed or pattern matched from outside the package, and `override ToString` renders the bare version for display. Every value comes out of `FantomasToolLocator.findFantomasTool` and is folded to the one shape daemons are cached under, which is what makes that shape a fact rather than a hope: the two places that resolve a version used to normalise separately, and had they ever drifted apart the same Fantomas would have got two daemon processes. Nothing in the API takes a version string, so a caller passes the value along to `daemonArgument` or `createForVersion` as it received it. [#3401](https://github.com/fsprojects/fantomas/pull/3401)
- The installed Fantomas is found with `dotnet tool list --format json` where the SDK offers it, which is 9.0.100 and later, rather than by reading the table that command prints for a person. That table's layout belongs to the SDK, and a change to it would stop Fantomas being found at all; the JSON names a schema version of its own. An SDK that refuses the flag is read from the table exactly as before, and which of the two applies is settled once per process. [#3401](https://github.com/fsprojects/fantomas/pull/3401)

### Fixed
- Opening a second folder that pins the same Fantomas version started a second daemon and dropped the first one from the cache without disposing it, leaving an orphaned process behind for the rest of the session. Daemons are keyed by version, so a running one for that version is now reused. [#3401](https://github.com/fsprojects/fantomas/pull/3401)
- A daemon that failed to start left every folder resolved to its version pinned to a version with no daemon behind it, which answered `CompatibleVersionIsKnownButNoDaemonIsRunning` for the rest of the session rather than trying again. Those folders are forgotten along with the version, so the next request resolves the tool from scratch. [#3401](https://github.com/fsprojects/fantomas/pull/3401)
- A Fantomas resolved from the `PATH` counted as a different version from the same Fantomas resolved from a `dotnet-tools.json`, and got a second daemon: `fantomas --version` prints `Fantomas v8.0.0+<commit>` where `dotnet tool list` prints `8.0.0`. [#3401](https://github.com/fsprojects/fantomas/pull/3401)
- When a daemon failed its handshake, the standard error quoted in the reported message was whatever had arrived by then rather than all of it, so the lines explaining the failure were usually missing. [#3401](https://github.com/fsprojects/fantomas/pull/3401)
- Two format requests in flight for the same file could deliver their configuration warnings in either order, so an empty one could clear problems that were still current. Nothing on the wire says which request a warning belongs to, so a client could not sort them out. The daemon now serves one request at a time per file; different files are unaffected. [#3401](https://github.com/fsprojects/fantomas/pull/3401)
- A daemon whose connection had ended was still handed out while its process lingered, so every request against it failed against a daemon the cache still believed in. Liveness now means the process is up and the connection has not ended. [#3401](https://github.com/fsprojects/fantomas/pull/3401)
- Replacing a crashed daemon started it the way whichever folder noticed the crash resolves now, rather than the way the daemon it replaces was started, so a daemon shared by two folders could come back under a different working directory. [#3401](https://github.com/fsprojects/fantomas/pull/3401)
- The version a tool manifest reports and the version `fantomas --version` prints are now folded to one casing before being compared, so a prerelease label written two ways no longer resolves to two daemons. [#3401](https://github.com/fsprojects/fantomas/pull/3401)
- The version handshake with a freshly started daemon had no timeout. A process that started but never answered held up every later request, because daemons are resolved on a single mailbox. It is now given 30 seconds, after which it is killed like any other failed handshake and reported as `Daemon did not answer the version request within 30000 ms.` rather than a bare timeout message. [#3401](https://github.com/fsprojects/fantomas/pull/3401)
- An unexpected error while resolving a daemon, such as a broken pipe while reading what `dotnet tool list` printed, ended the mailbox that every folder is served from. Nothing reported it, and because a request waits on that mailbox without a timeout, formatting stopped answering at all for the rest of the session rather than failing. Such an error is now reported like any other, and the daemons already running are kept. [#3401](https://github.com/fsprojects/fantomas/pull/3401)

## [0.11.0] - 2026-04-16

### Changed
- Bump `FSharp.Core` to `10.0.100`.
- Bump `StreamJsonRpc` to `2.24.84`.
- Bump `SemanticVersioning` to `3.0.0`.

## [0.10.0] - 2025-01-13

### Changed
- Bump packages

## [0.9.1] - 2024-08-19

### Fixed
- Fantomas.Client does not respect DOTNET_CLI_HOME env variable. [#3104](https://github.com/fsprojects/fantomas/issues/3104)

## [0.9.0] - 2023-02-24

### Fixed
- Fix JSON serialization of new cursor API. [#2778](https://github.com/fsprojects/fantomas/issues/2778)

## [0.8.0] - 2023-01-24

### Changed
- Initial cursor API. [#2739](https://github.com/fsprojects/fantomas/pull/2739)

## [0.7.0] - 2022-11-09

### Changed
- Changed `FormatSelectionRange` to class instead of struct.

## [0.6.0] - 2022-06-27

### Changed
- Add `SelectedRange` to `FantomasResponse`.

## [0.5.4] - 2022-05-06

### Changed
- FSharp.Core 5.0.1 or higher. [#2227](https://github.com/fsprojects/fantomas/pull/2227)

## [0.5.3] - 2022-05-06

### Changed
- Lower StreamJsonRpc to match 2.8.28. [#2227](https://github.com/fsprojects/fantomas/pull/2227)

## [0.5.2] - 2022-05-06

### Fixed
- Include prerelease when verifying a compatible version. [#2227](https://github.com/fsprojects/fantomas/pull/2227)

## [0.5.1] - 2022-02-01
