---
category: Contributors
categoryindex: 2
index: 18
---
# Native AOT

On Linux x64 the command line tool ships compiled with Native AOT: one native executable, with no
JIT and no runtime code generation ([#3171](https://github.com/fsprojects/fantomas/issues/3171)).
Code that works on the JIT can fail there, and often only on some paths and in some builds, so the
tool keeps to a few rules, and a pipeline checks the tool the way a user runs it.

## What code the tool runs cannot use

These apply to `src/Fantomas` and `src/Fantomas.Core`. The tests, `Fantomas.Client` and the build
scripts only ever run on the JIT.

- **printf.** `sprintf`, `failwithf`, `%A` and interpolated strings with anything but a bare hole
  build their formatters at runtime. [FANTOMAS-PRINTF-001](https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-printf-001)
  has the full rule and what to write instead. The analyzer reports it while you type, and
  `nothing the tool runs goes through printf` in `Fantomas.Tests` checks the compiled assemblies.
  To write a value out in full for a bug report, use `Triage.dump`: it tries `%A`, and falls back
  to the type's name where Native AOT cannot run it.
- **F# reflection.** `FSharpValue` and `FSharpType` work or not under Native AOT depending on what
  the trimmer kept, and that changes from one build to the next. Match on the cases instead. To name
  a union case in a message, `UnionCase.name` does it without depending on reflection.
- **Serialization by reflection.** The daemon is the one place that serializes, see below.

## The daemon

StreamJsonRpc finds the daemon's methods and serializes its messages by reflection by default.
Under Native AOT two pieces replace that:

- `src/Fantomas.Daemon.Contract` is a small C# project, in C# for PolyType's source generator alone.
  `IFantomasDaemon` lists the methods the daemon answers, and the shape PolyType generates for it
  is what StreamJsonRpc dispatches through.
- `src/Fantomas/DaemonJson.fs` has a converter, written by hand, for each request and response. They
  write the format `Fantomas.Client` reads with Newtonsoft.Json: a union case is
  `{"Case": ..., "Fields": [...]}`, and an option is `null` or the case `Some`.

To change a request or a response type in `Fantomas.Client`, change its converter too. The compiler
holds you to it: a converter matches on every case of a response, and an incomplete match is an
error in this repository, and it builds every request record in full. A new method of the daemon
goes on `IFantomasDaemon` as well.

`DaemonWireTests` pins the format. Each case under `src/Fantomas.Tests/Integration/DaemonWire` is a
`<case>.json`, the requests to send, and a `<case>.gold`, the conversation that results. A changed
conversation fails the test and writes `<case>.received.gold` beside the gold. Rename it over the
gold to accept it, or run `dotnet fsi build.fsx -- -p UpdateSnapshots` to accept every change. A
change to a gold is a change editors see, so make it on purpose.

## Checking the tool under Native AOT

```bash
dotnet fsi build.fsx -- -p TestAot
```

It does four things:

1. It publishes the tool with Native AOT to `artifacts/publish/aot/<rid>`. It clears the native
   intermediates first: an AOT compile that is up to date is skipped, and then it logs no warnings.
2. It fails on any trim or AOT warning of that publish that `knownAotWarnings` in `build.fsx` does
   not list.
3. It runs the tests of `Fantomas.Tests` against the native executable, through
   `FANTOMAS_EXECUTABLE`. That variable points every test that starts the tool at another build.
4. It publishes `Fantomas.Core.AotSmokeTest` the same way and runs it. It calls the public API of
   the library the way a consumer such as fantomas-tools does, and fails on a check that does not
   hold.

The `Native AOT` job of CI runs it on Linux, Windows and macOS, because Native AOT only compiles
for the machine it runs on, and `Native AOT (Alpine)` runs it in an Alpine container, for musl.

When it fails on a new warning, find where the warning points. If the tool can reach that code,
change the code. If it cannot, add an entry to `knownAotWarnings` with the reason, which is what
makes the next new warning stand out.

## The `FantomasAot` switch

Everything specific to Native AOT is behind the MSBuild property `FantomasAot`, which the `TestAot`
and `Release` pipelines set. A normal build and restore stay as they are without it.

- `src/Fantomas/Fantomas.fsproj` publishes with Native AOT, packs one package per platform, and
  leaves the native debug symbols and the XML documentation out of the package.
- `Directory.Build.props` writes the lock files of an AOT restore under `artifacts/obj`. That
  restore is for a runtime identifier, which the lock files of the repository do not record.
- `Fantomas.Core.AotSmokeTest.fsproj` publishes with Native AOT.
- `Fantomas.Daemon.Contract.csproj` turns on the AOT analyzers.

## Packages and the release

The tool ships as four packages:

- `fantomas` only names the three below. `dotnet tool install fantomas` reads it and installs one of
  them.
- `fantomas.linux-x64` holds the native executable for glibc, from `dotnet pack -p:FantomasAot=true`.
- `fantomas.linux-musl-x64` holds the native executable for musl, as on Alpine, from
  `dotnet pack -r linux-musl-x64 -p:FantomasAot=true` on Alpine.
- `fantomas.any` is the tool as it shipped before Native AOT, from
  `dotnet pack -r any -p:FantomasAot=true`. Every platform without a package of its own installs it.

The SDK only packs the platforms the machine it runs on can compile for, and fails with MSB4006 on a
top-level package that names none of them. So the release workflow has a job per machine:

- `musl` runs the `PackNativeTool` pipeline in an Alpine container, and hands
  `fantomas.linux-musl-x64` on as an artifact.
- `release` runs the `Release` pipeline on Ubuntu. Its `PackTool` stage packs the other three, copies
  the musl package in from `artifacts/native-tool-packages`, and fails when `fantomas` names a package
  that is not there, as `dotnet tool install` would fail on that platform.

A native package for another platform takes a job like `musl` on a machine of that platform, and its
identifier in `ToolPackageRuntimeIdentifiers`.

A package that names a package per platform is new in the .NET 10 SDK, so installing the tool takes
that SDK or a newer one. An older SDK refuses `fantomas` with "Format version is higher than
supported", and `fantomas.any` as well. Up to v8 an older SDK could install the tool, as long as the
runtime it targets was there.

## Where the native executable runs

`dotnet tool install fantomas` picks `fantomas.linux-x64` on Linux x64, and
`fantomas.linux-musl-x64` where the SDK is the one for musl, as on Alpine.

- **`fantomas.linux-x64` needs glibc 2.35 or newer.** An executable asks for the glibc version that
  the machine it was linked on had, function by function. The release runs on `ubuntu-22.04` for
  that reason, which has glibc 2.35, and the Linux run of the `Native AOT` job does too, so its tests
  run the executable that ships. A newer runner would raise that: `ubuntu-24.04` links against
  glibc 2.38. RHEL 9 and Amazon Linux 2023 have glibc 2.34.
- **`fantomas.linux-musl-x64` is compiled on Alpine 3.22,** and runs on 3.20 and every version
  since.
- **Both need ICU,** as the .NET runtime does. A machine with the .NET SDK on it has it.

Where it cannot run, `dotnet tool install fantomas.any` installs the portable tool by name. Its
command is `fantomas` as well.

## Settings to look at again

Some settings are there for the SDK or the libraries as they are now. Each says why beside it, and
each can go when the reason does.

- **`UseOptimizedPublish` is false** in `Directory.Build.props`. The .NET 11 SDK publishes for
  Native AOT without a full build, so nothing reaches `bin`, but it still copies the XML
  documentation from there and fails with MSB3030. After an SDK update, remove the setting and run
  `TestAot` and a pack. If both pass, it can stay out.
- **The AOT analyzers only run with `FantomasAot`.** `IsAotCompatible` adds a reference to
  `Microsoft.NET.ILLink.Tasks` at the newest .NET 10 patch the SDK knows, so every SDK update changes
  a lock file that has it, and a locked restore fails with NU1004. This stays as long as the
  repository uses lock files.
- **`knownAotWarnings`** in `build.fsx`. The `FSharp.Core` entries can go when FSharp.Core is clean
  for trimming. The `sr.fs` entries can go when the vendored compiler no longer has
  `DiagnosticMessage.mkFunctionValue`.
- **The release runs on `ubuntu-22.04`**, for the glibc it links against. When GitHub retires that
  runner, the next one raises the glibc the executable needs, unless the release compiles it in a
  container with an older glibc.
- **FANTOMAS-PRINTF-001 reads the untyped tree** until the analyzer SDK ships a compiler with
  dotnet/fsharp#19971. Its section in `analyzers/AGENTS.md` has the details.

<fantomas-nav source="{{fsdocs-source-filename}}" previous="{{fsdocs-previous-page-link}}" next="{{fsdocs-next-page-link}}"></fantomas-nav>
