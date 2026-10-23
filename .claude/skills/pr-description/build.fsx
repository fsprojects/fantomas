// Builds one commit of this repository in tmp/pr-description/<sha>/, leaving the checkout alone.
//
//   dotnet fsi build.fsx <sha> [project]
//
// project defaults to src/Fantomas.Core.SnapshotTests; pass src/Fantomas/Fantomas.fsproj for the
// command line tool. A tree already built for that project is not built again. Prints the tree's
// path on the last line.
#load "marker.fsx"

open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

let repo: string =
    Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", ".."))

let run (workingDirectory: string) (log: string) (file: string) (arguments: string list) : unit =
    let info: ProcessStartInfo =
        ProcessStartInfo(file, arguments, WorkingDirectory = workingDirectory)

    info.RedirectStandardOutput <- true
    info.RedirectStandardError <- true
    use proc: Process = Process.Start info
    let stdout = proc.StandardOutput.ReadToEndAsync()
    let stderr = proc.StandardError.ReadToEndAsync()
    proc.WaitForExit()
    File.AppendAllText(log, $"$ %s{file} %s{String.Join(' ', arguments)}\n%s{stdout.Result}%s{stderr.Result}\n")

    if proc.ExitCode <> 0 then
        eprintfn $"'%s{file} %s{String.Join(' ', arguments)}' failed in %s{workingDirectory}, see %s{log}"
        exit 1

let output (file: string) (arguments: string list) : string =
    let info: ProcessStartInfo =
        ProcessStartInfo(file, arguments, WorkingDirectory = repo)
    info.RedirectStandardOutput <- true
    use proc: Process = Process.Start info
    let text: string = proc.StandardOutput.ReadToEnd()
    proc.WaitForExit()
    if proc.ExitCode <> 0 then exit 1 else text.Trim()

let rec copyDirectory (source: string) (target: string) : unit =
    Directory.CreateDirectory target |> ignore

    for file in Directory.GetFiles source do
        File.Copy(file, Path.Combine(target, Path.GetFileName file), true)

    for directory in Directory.GetDirectories source do
        copyDirectory directory (Path.Combine(target, Path.GetFileName directory))

let sha, project =
    match fsi.CommandLineArgs[1..] with
    | [| sha |] -> sha, "src/Fantomas.Core.SnapshotTests"
    | [| sha; project |] -> sha, project
    | _ ->
        eprintfn "usage: dotnet fsi build.fsx <sha> [project]"
        exit 1

// A sha the checkout does not have makes this exit: the agent asks the user to fetch it.
let commit: string = output "git" [ "rev-parse"; "--verify"; $"%s{sha}^{{commit}}" ]
let root: string = Path.Combine(repo, "tmp", "pr-description")
let tree: string = Path.Combine(root, commit)
let log: string = Path.Combine(root, $"%s{commit}.log")

if not (Marker.isBuilt tree project) then
    if not (File.Exists(Path.Combine(tree, "Directory.Build.props"))) then
        Directory.CreateDirectory tree |> ignore
        let archive: string = Path.Combine(root, $"%s{commit}.tar")
        run repo log "git" [ "archive"; "--format=tar"; "-o"; archive; commit ]
        run tree log "tar" [ "-xf"; archive ]
        File.Delete archive

    // .deps is ignored by git, so the archive lacks the vendored compiler sources.
    let props: string = File.ReadAllText(Path.Combine(tree, "Directory.Build.props"))
    let hash: string =
        Regex.Match(props, "<FCSCommitHash>([0-9a-f]+)</FCSCommitHash>").Groups[1].Value

    if hash = "" then
        eprintfn $"%s{commit} names no FCSCommitHash in Directory.Build.props: it cannot be built this way."
        exit 1
    let local: string = Path.Combine(repo, ".deps", hash)
    let target: string = Path.Combine(tree, ".deps", hash)

    if not (Directory.Exists target) then
        if Directory.Exists local then
            copyDirectory local target
        else
            run tree log "dotnet" [ "tool"; "restore" ]
            run tree log "dotnet" [ "fsi"; "build.fsx"; "--"; "-p"; "Init" ]

    // The tree sits inside this checkout, so asking git would stamp the checkout's HEAD into the
    // version a build prints; the tree's own commit is given instead.
    run
        tree
        log
        "dotnet"
        [
            "build"
            project
            "-p:EnableSourceControlManagerQueries=false"
            $"-p:SourceRevisionId=%s{commit}"
        ]
    File.WriteAllText(Marker.path tree project, Marker.recipe)

// Copied on every run, so a change to the format script reaches trees built before it. A tree from
// before scripts/shared.fsx (February 2026) has nothing for it to load, and is measured in cli mode.
if File.Exists(Path.Combine(tree, "scripts", "shared.fsx")) then
    File.Copy(
        Path.Combine(__SOURCE_DIRECTORY__, "format.fsx"),
        Path.Combine(tree, "scripts", "pr-description-format.fsx"),
        true
    )

printfn $"%s{tree}"
