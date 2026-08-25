#r "nuget: CliWrap, 3.6.4"

open System.IO
open System.Text.RegularExpressions
open CliWrap
open CliWrap.Buffered
// Loaded by `build.fsx`, after `BuildCommon.fsx`. An error here saying BuildCommon is not defined
// means this file was run on its own; it is a library, so run a pipeline from build.fsx instead.
open BuildCommon

// Compiling this repository's own scripts without running them.
//
// Nothing else in the build looks at them, so a rename in `src/` that one of them refers to breaks
// it silently: the script keeps sitting there and fails the next time somebody reaches for it,
// which is usually in the middle of something else. The documentation scripts have exactly that
// problem too, and both of the ones that `#load` a file out of `src/` were broken this way.
//
// They fall into two groups, and the difference is which build they reference: the scripts beside
// this file take the debug build, the documentation takes the release one. That is why there are
// two entry points here rather than one, run from two different points of the pipeline.

/// The project the diagnostic scripts are compiled against.
///
/// Anchored at `repositoryRoot` rather than written relative, so it names the same project whatever
/// the working directory of the run is.
///
/// `shared.fsx` references the debug build of Fantomas.Core, and Fantomas.Core references
/// Fantomas.FCS, so building this one project puts both assemblies where the scripts look for them.
/// The CLI and the test projects are no part of what a script loads and are not built for this.
///
/// It is the debug build they reference, and that is not a detail to tidy away into the release
/// build the rest of the pipeline makes: these scripts are for prototyping against a local
/// Fantomas, which is something you want to be able to step through.
let scriptProject: string =
    repositoryRoot </> "src" </> "Fantomas.Core" </> "Fantomas.Core.fsproj"

/// Of the given scripts, the ones that are meant to be run directly.
///
/// A script that another of them `#load`s is left out, because it is already compiled as part of
/// whatever loads it. Several cannot be compiled alone at all, by design: this file and its
/// neighbours expect `BuildCommon.fsx` to be in scope, which is only true when `build.fsx` did the
/// loading. Reading the `#load` lines rather than listing those exceptions means a script added
/// later is checked without anything here having to be edited.
let private runnableIn (scripts: string list) : string list =
    let loadDirective: Regex = Regex("^\\s*#load\\s+\"([^\"]+)\"")

    let loaded: Set<string> =
        scripts
        |> Seq.collect (fun (script: string) ->
            let folder: string = Path.GetDirectoryName script

            File.ReadLines script
            |> Seq.choose (fun (line: string) ->
                let matched: Match = loadDirective.Match line

                if matched.Success then
                    Some(Path.GetFullPath(folder </> matched.Groups[1].Value))
                else
                    None))
        |> Set.ofSeq

    scripts
    |> List.filter (fun (script: string) -> not (loaded.Contains(Path.GetFullPath script)))

/// `build.fsx` and the diagnostic scripts beside this file, which reference the debug build.
let runnableScripts () : string list =
    [
        repositoryRoot </> "build.fsx"
        yield! Directory.EnumerateFiles(repositoryRoot </> "scripts", "*.fsx")
    ]
    |> runnableIn

/// The documentation scripts, which fsdocs turns into the pages of the site. They reference the
/// release build, so they can only be compiled once the pipeline has made one.
let runnableDocScripts () : string list =
    Directory.EnumerateFiles(repositoryRoot </> "docs", "*.fsx", SearchOption.AllDirectories)
    |> List.ofSeq
    |> runnableIn

/// Compile one script and stop short of running it, reporting whatever the compiler said.
let private typecheckScript (script: string) : Async<string * int * string> =
    async {
        let! result =
            Cli
                .Wrap("dotnet")
                .WithArguments($"fsi --typecheck-only --nologo \"{script}\"")
                .WithWorkingDirectory(repositoryRoot)
                .WithValidation(CommandResultValidation.None)
                .ExecuteBufferedAsync()
                .Task
            |> Async.AwaitTask

        return script, result.ExitCode, (result.StandardOutput + result.StandardError).Trim()
    }

/// Compile each of the given scripts, and report what the compiler said about any that would not
/// compile. Writes nothing: no script is run, and the assemblies they reference are built by the
/// stage before whichever one calls this.
let private check (scripts: string list) : Async<int> =
    async {
        // One at a time: the compiler output of a script that fails is the point of this, and
        // running them together interleaves it beyond reading.
        let! results = scripts |> List.map typecheckScript |> Async.Sequential

        for (script: string), (exitCode: int), (output: string) in results do
            let name: string = Path.GetRelativePath(repositoryRoot, script)

            if exitCode = 0 then
                printfn "%s compiles." name
            else
                printfn "%s does not compile:" name
                printfn "%s" output

        let failed: int =
            results |> Array.filter (fun (_, exitCode, _) -> exitCode <> 0) |> Array.length

        return (if failed = 0 then 0 else 1)
    }

/// Compile `build.fsx` and the diagnostic scripts. Needs the debug build.
let checkScripts _ : Async<int> = check (runnableScripts ())

/// Compile the documentation scripts. Needs the release build.
let checkDocScripts _ : Async<int> = check (runnableDocScripts ())
