#r "nuget: CliWrap, 3.6.4"

open System
open System.IO
open CliWrap
open CliWrap.Buffered

// This file is loaded by `build.fsx`. It defines things and runs nothing, so a direct run would
// look like a success while doing no work at all. Say so instead.
if Path.GetFileName(fsi.CommandLineArgs[0]) = Path.GetFileName __SOURCE_FILE__ then
    eprintfn "%s is loaded by build.fsx and is not meant to be run on its own." (Path.GetFileName __SOURCE_FILE__)
    eprintfn "Run a pipeline instead, for example: dotnet fsi build.fsx -- -p Build"
    exit 1

// What every part of the build agrees on: where things are, how to run a process, and what the
// working tree changed.
//
// Anything here is used by at least two of `build.fsx`, `BuildAnalyzers.fsx`, `BuildRelease.fsx` and
// `BuildCompiler.fsx`. Anything used by only one of them belongs in that one.

let (</>) a b = Path.Combine(a, b)

/// The repository root.
///
/// `__SOURCE_DIRECTORY__` is the folder of the file it is written in, which here is `scripts/` and
/// not the root. Every path below is anchored to this instead, so that what the build points at does
/// not depend on which script did the loading.
let repositoryRoot: string = Path.GetFullPath(__SOURCE_DIRECTORY__ </> "..")

let artifactsDir: string = repositoryRoot </> "artifacts"
let binDir: string = artifactsDir </> "bin"
let packagesDir: string = artifactsDir </> "package" </> "release"
let coverageReportDir: string = repositoryRoot </> "coveragereport"

/// Where the analyzers write their report per project, before the reports are merged into one.
let analysisReportsDir: string = repositoryRoot </> "analysisreports"

/// The merged analyzer report. Holds the last run and nothing more.
let mergedAnalysisReport: string = repositoryRoot </> "analysis.sarif"

/// Deleting a folder can fail with "Directory not empty" when something writes into it while
/// the delete is walking it, Finder dropping a .DS_Store back in is enough. The delete does
/// remove what it got to, so retry a couple of times before giving up.
let rec private deleteDirectory (attempt: int) (dir: string) : Async<unit> =
    async {
        try
            Directory.Delete(dir, true)
        with :? IOException when attempt < 5 ->
            do! Async.Sleep(100 * attempt)

            if Directory.Exists(dir) then
                return! deleteDirectory (attempt + 1) dir
    }

let cleanFolders (input: string seq) : Async<unit> =
    async {
        for dir in input do
            if Directory.Exists(dir) then
                do! deleteDirectory 1 dir
    }

let runGitCommand (arguments: string) =
    async {
        let! result =
            Cli.Wrap("git").WithArguments(arguments).WithWorkingDirectory(repositoryRoot).ExecuteBufferedAsync().Task
            |> Async.AwaitTask

        return result.ExitCode, result.StandardOutput, result.StandardError
    }

let runCmd file (arguments: string) =
    async {
        let! result = Cli.Wrap(file).WithArguments(arguments).ExecuteAsync().Task |> Async.AwaitTask
        return result.ExitCode
    }

/// The files git reports as changed in the working tree, as paths relative to the repository root.
///
/// The porcelain format is two status columns, a space, and then the path, so the path starts at
/// the fourth character. A rename reads as `old -> new`, of which only the new path still exists.
/// Deleted files are dropped: there is nothing left to look at.
///
/// Untracked files are asked for one by one. Git otherwise reports a new folder as a single entry
/// and the files inside it are never named, which is exactly the case of a feature that arrives as
/// a new folder of sources.
let changedFiles () : Async<string list> =
    async {
        let! exitCode, stdout, stdErr = runGitCommand "status --porcelain --untracked-files=all"

        if exitCode <> 0 then
            failwith $"Could not read the git status.\n{stdErr}"

        return
            stdout.Split('\n')
            |> Array.choose (fun (line: string) ->
                let line: string = line.TrimEnd('\r')

                if line.Length < 4 || line[0] = 'D' || line[1] = 'D' then
                    None
                else
                    let path: string = line.Substring 3

                    let path: string =
                        match path.IndexOf(" -> ", StringComparison.Ordinal) with
                        | -1 -> path
                        | arrow -> path.Substring(arrow + 4)

                    Some(path.Trim('"').Replace('\\', '/')))
            |> List.ofArray
    }

let hasExtension (extensions: string list) (path: string) : bool =
    extensions
    |> List.exists (fun (extension: string) -> path.EndsWith(extension, StringComparison.Ordinal))

/// How much of a file the working tree touched.
type ChangedLines =
    /// Every line, which is what a file that git has never seen amounts to.
    | WholeFile
    /// The lines a diff hunk added or altered.
    | Lines of Set<int>

/// The lines the working tree changed, per file, keyed by repository relative path.
///
/// `git diff HEAD` covers staged and unstaged changes alike, and `-U0` asks for no context lines,
/// so every hunk header names exactly the lines that differ. An untracked file has no diff to read
/// and is new in its entirety.
let changedLines () : Async<Map<string, ChangedLines>> =
    async {
        let! files = changedFiles ()
        let! exitCode, stdout, stdErr = runGitCommand "diff -U0 HEAD --"

        if exitCode <> 0 then
            failwith $"Could not read the git diff.\n{stdErr}"

        let hunk: Text.RegularExpressions.Regex =
            Text.RegularExpressions.Regex(@"^@@ -\S+ \+(?<start>\d+)(,(?<count>\d+))? @@")

        let mutable scopes: Map<string, ChangedLines> = Map.empty
        let mutable current: string option = None

        for line in stdout.Split('\n') do
            let line: string = line.TrimEnd('\r')

            if line.StartsWith("+++ b/", StringComparison.Ordinal) then
                current <- Some(line.Substring 6)
            elif line.StartsWith("+++ ", StringComparison.Ordinal) then
                current <- None
            else
                let m: Text.RegularExpressions.Match = hunk.Match line

                match current with
                | None -> ()
                | Some file when m.Success ->
                    let start: int = int m.Groups["start"].Value

                    let count: int =
                        if m.Groups["count"].Success then
                            int m.Groups["count"].Value
                        else
                            1

                    // A pure deletion reports a count of zero. Nothing of it survives to report on.
                    let added: Set<int> = set [ start .. start + count - 1 ]

                    let merged: ChangedLines =
                        match Map.tryFind file scopes with
                        | Some(Lines existing) -> Lines(Set.union existing added)
                        | _ -> Lines added

                    scopes <- Map.add file merged scopes
                | Some _ -> ()

        // Anything git named as changed but has no diff hunk is untracked, so all of it is new.
        for file in files do
            if not (Map.containsKey file scopes) then
                scopes <- Map.add file WholeFile scopes

        return scopes
    }

/// How much of the file a finding sits in the working tree touched, or `None` when it touched none
/// of it.
///
/// `changedLines` puts an entry in the map for every file git named, so a miss here is a fact and
/// not an absence of information: git was asked, and said this file did not change.
let scopeFor (scopes: Map<string, ChangedLines>) (path: string) : ChangedLines option =
    let normalized: string = path.Replace('\\', '/')

    scopes
    |> Map.tryPick (fun (file: string) (scope: ChangedLines) ->
        if normalized.EndsWith(file, StringComparison.Ordinal) then
            Some scope
        else
            None)
