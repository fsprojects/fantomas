// Runs the same input through the base build and the head build, and diffs the two outputs.
//
//   dotnet fsi compare.fsx <n> <name> <base> <head> format <input> [flags...]
//   dotnet fsi compare.fsx <n> <name> <base> <head> cli <sample-folder> [arguments...]
//
// format: the side's scripts/pr-description-format.fsx on <input>; flags are its own
//         (--editorconfig <content>, --signature). A snapshot case is read as it is.
// cli:    the side's fantomas.dll with <arguments>, in a fresh copy of <sample-folder>. Both sides
//         run in the same folder, and its absolute path is printed as `sample`, so the output
//         holds no local path and does not differ between the sides.
//
// Writes out/<name>.base, .head, their .status and .diff, and in cli mode .files.diff (each file
// named under `sample`), under
// tmp/pr-description/<n>/, saves this command as commands/<name>.sh there, and prints both statuses
// and the diff. With PR_DESCRIPTION_RERUN set, as a rerun of those commands sets it, the command is
// not saved again.
#load "marker.fsx"

open System
open System.Diagnostics
open System.IO

let repo: string =
    Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "..", ".."))

let arguments: string array = fsi.CommandLineArgs[1..]

if arguments.Length < 6 then
    eprintfn "usage: dotnet fsi compare.fsx <n> <name> <base> <head> (format <input>|cli <sample-folder>) [...]"
    exit 1

let number, name, baseSha, headSha, mode, subject =
    arguments[0], arguments[1], arguments[2], arguments[3], arguments[4], Path.GetFullPath arguments[5]

let rest: string list = List.ofArray arguments[6..]

let quote (a: string) : string = "'" + a.Replace("'", "'\\''") + "'"

type Captured =
    {
        Stdout: string
        Stderr: string
        ExitCode: int
    }

let capture (workingDirectory: string) (file: string) (args: string list) : Captured =
    let info: ProcessStartInfo =
        ProcessStartInfo(file, args, WorkingDirectory = workingDirectory)

    info.RedirectStandardOutput <- true
    info.RedirectStandardError <- true
    use proc: Process = Process.Start info
    let stdout = proc.StandardOutput.ReadToEndAsync()
    let stderr = proc.StandardError.ReadToEndAsync()
    proc.WaitForExit()

    {
        Stdout = stdout.Result
        Stderr = stderr.Result
        ExitCode = proc.ExitCode
    }

let resolve (sha: string) : string =
    let found: Captured =
        capture repo "git" [ "rev-parse"; "--verify"; $"%s{sha}^{{commit}}" ]

    let tree: string = Path.Combine(repo, "tmp", "pr-description", found.Stdout.Trim())

    // cli mode runs the tool, which only a build of src/Fantomas/Fantomas.fsproj produces. format
    // mode runs the tree's scripts, which load the snapshot tests' assembly where the tree has them,
    // and otherwise only what a build of the tool also builds.
    let hasSnapshotTests: bool =
        (capture repo "git" [ "cat-file"; "-e"; $"%s{found.Stdout.Trim()}:src/Fantomas.Core.SnapshotTests" ]).ExitCode =
            0

    let project: string =
        if mode = "cli" || not hasSnapshotTests then
            "src/Fantomas/Fantomas.fsproj"
        else
            "src/Fantomas.Core.SnapshotTests"

    if found.ExitCode <> 0 || not (Marker.isBuilt tree project) then
        eprintfn $"No %s{mode} build for %s{sha}: run build.fsx %s{sha} %s{project} first."
        exit 1

    tree

let work: string = Path.Combine(repo, "tmp", "pr-description", number)
let out: string = Path.Combine(work, "out")
Directory.CreateDirectory out |> ignore

let rec copyDirectory (source: string) (target: string) : unit =
    Directory.CreateDirectory target |> ignore

    for file in Directory.GetFiles source do
        File.Copy(file, Path.Combine(target, Path.GetFileName file), true)

    for directory in Directory.GetDirectories source do
        copyDirectory directory (Path.Combine(target, Path.GetFileName directory))

let side (label: string) (tree: string) : string * string =
    match mode with
    | "format" ->
        let script: string = Path.Combine(tree, "scripts", "pr-description-format.fsx")

        let captured: Captured =
            capture tree "dotnet" ([ "fsi"; script ] @ rest @ [ subject ])

        captured.Stdout, captured.Stderr
    | "cli" ->
        // Both sides run in run/<name>, so the paths they print agree; each side's files are
        // then kept in run/<name>.<side>.
        let folder: string = Path.Combine(work, "run", name)
        let kept: string = $"%s{folder}.%s{label}"

        for each in [ folder; kept ] do
            if Directory.Exists each then
                Directory.Delete(each, true)

        copyDirectory subject folder

        let dll: string =
            Path.Combine(tree, "artifacts", "bin", "Fantomas", "debug", "fantomas.dll")
        // Through sh, so stdout and stderr keep the order they were written in.
        let command: string = String.Join(" ", "dotnet" :: dll :: rest |> List.map quote)
        let captured: Captured = capture folder "sh" [ "-c"; command + " 2>&1" ]
        Directory.Move(folder, kept)
        captured.Stdout.Replace(folder, "sample"), $"-- exit %d{captured.ExitCode}\n"
    | other ->
        eprintfn $"Unknown mode '%s{other}': use format or cli."
        exit 1

// Both trees are resolved before either side runs, and nothing is written to out/ until both ran:
// a failed measurement leaves no files behind for the rerun in step 6 to miss.
let trees: (string * string) list =
    [ "base", resolve baseSha; "head", resolve headSha ]

let results: (string * (string * string)) list =
    trees |> List.map (fun (label: string, tree: string) -> label, side label tree)

// Every status the runners write starts with "-- "; anything else is the runner itself failing,
// such as a tree whose scripts/shared.fsx no longer compiles. A crash prints "--- End of stack
// trace" lines, so only a line that starts with "-- " counts.
for label: string, (_, status: string) in results do
    let ran: bool =
        status.Split('\n') |> Array.exists (fun (line: string) -> line.StartsWith "-- ")

    if not ran then
        eprintfn $"The %s{label} side did not run:\n%s{status.Trim()}"
        exit 1

// What an earlier measurement under this name left, such as a cli run's files.diff.
for suffix in [ ".base"; ".head"; ".base.status"; ".head.status"; ".diff"; ".files.diff" ] do
    File.Delete(Path.Combine(out, name + suffix))

for label: string, (result: string, status: string) in results do
    File.WriteAllText(Path.Combine(out, $"%s{name}.%s{label}"), result)
    File.WriteAllText(Path.Combine(out, $"%s{name}.%s{label}.status"), status)

let diff: Captured =
    capture
        out
        "git"
        [
            "diff"
            "--no-index"
            "--no-color"
            "--no-ext-diff"
            "-U9999"
            $"%s{name}.base"
            $"%s{name}.head"
        ]

// git diff exits 0 for no difference and 1 for a difference; anything else is git failing.
if diff.ExitCode > 1 then
    eprintfn $"git diff failed:\n%s{diff.Stderr.Trim()}"
    exit 1

// Drop the header, up to and including the first hunk line.
let body: string =
    diff.Stdout.Split('\n')
    |> Array.skipWhile (fun (line: string) -> not (line.StartsWith "@@"))
    |> Array.skip (if diff.ExitCode = 0 then 0 else 1)
    |> String.concat "\n"

File.WriteAllText(Path.Combine(out, $"%s{name}.diff"), body)

// In cli mode the tool may also have rewritten the sample's files: diff what each side left.
let files: string option =
    if mode = "cli" then
        let run: string = Path.Combine(work, "run")

        let captured: Captured =
            capture
                run
                "git"
                [
                    "diff"
                    "--no-index"
                    "--no-color"
                    "--no-ext-diff"
                    "--no-prefix"
                    "-U9999"
                    $"%s{name}.base"
                    $"%s{name}.head"
                ]

        if captured.ExitCode > 1 then
            eprintfn $"git diff failed:\n%s{captured.Stderr.Trim()}"
            exit 1

        // Each file's --- and +++ lines name it under `sample`, as the tool's output does, so the diff
        // still says which file changed but holds no local folder. The diff --git and index lines add
        // nothing to that.
        let prefixes: (string * string) list =
            [ $"--- %s{name}.base/", "--- sample/"; $"+++ %s{name}.head/", "+++ sample/" ]

        let rewrite (line: string) : string =
            prefixes
            |> List.tryFind (fun (prefix: string, _) -> line.StartsWith prefix)
            |> Option.map (fun (prefix: string, replacement: string) -> replacement + line.Substring prefix.Length)
            |> Option.defaultValue line

        let text: string =
            captured.Stdout.Split('\n')
            |> Array.filter (fun (line: string) -> not (line.StartsWith "diff --git " || line.StartsWith "index "))
            |> Array.map rewrite
            |> String.concat "\n"

        File.WriteAllText(Path.Combine(out, $"%s{name}.files.diff"), text)
        Some text
    else
        None

if String.IsNullOrEmpty(Environment.GetEnvironmentVariable "PR_DESCRIPTION_RERUN") then
    let line: string =
        String.Join(
            " ",
            "dotnet fsi"
            :: quote (Path.Combine(__SOURCE_DIRECTORY__, "compare.fsx"))
            :: List.map quote (List.ofArray arguments[0..4] @ [ subject ] @ rest)
        )

    let commands: string = Path.Combine(work, "commands")
    Directory.CreateDirectory commands |> ignore
    File.WriteAllText(Path.Combine(commands, $"%s{name}.sh"), line + "\n")

let read (suffix: string) : string =
    File.ReadAllText(Path.Combine(out, name + suffix))

printfn $"== %s{name}"
printfn $"""base: %s{(read ".base.status").TrimEnd()}"""
printfn $"""head: %s{(read ".head.status").TrimEnd()}"""

if diff.ExitCode = 0 then
    printfn "-- no difference; both sides gave:"
    printf $"""%s{read ".head"}"""
else
    printfn "```diff"
    printf $"%s{body}"
    printfn "```"

match files with
| Some "" -> printfn "-- the files each side left are the same"
| Some text ->
    printfn "-- the files each side left differ:"
    printfn "```diff"
    printf $"%s{text}"
    printfn "```"
| None -> ()
