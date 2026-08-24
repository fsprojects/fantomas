#r "nuget: CliWrap, 3.6.4"
#r "nuget: FSharp.Data, 6.3.0"

open System
open System.IO
open System.Xml.Linq
open System.Xml.XPath
open CliWrap
open CliWrap.Buffered
open FSharp.Data
// Loaded by `build.fsx`, after `BuildCommon.fsx`. An error here saying BuildCommon is not defined
// means this file was run on its own; it is a library, so run a pipeline from build.fsx instead.
open BuildCommon

// Running the analyzers, and deciding which of their findings a run set out to report.
//
// The pipelines reach all of this through a handful of names: `projectsToAnalyze`, `targetsFor`,
// `analyzeTargets` and the two filters. Everything between those and the SARIF on disk is detail,
// and detail that grew every time the reporting was made more honest.

/// The projects the analyzers run over: every project in the solution, minus the ones whose source
/// is not ours to change. Fantomas.FCS is generated from the vendored compiler sources, and
/// Fantomas.FCS.BuildTasks compiles a single vendored compiler file, so a finding in either is
/// something to report upstream rather than something to fix here. Reading the solution rather than
/// globbing keeps the rest of the build tooling out.
///
/// This includes the analyzers themselves, which are in the solution like everything else. There is
/// nothing circular about a rule reporting on the project that defines it: the pipelines build the
/// analyzers before running them, so what looks at this code is the build the run started with.
let projectsToAnalyze: string list =
    let excluded = set [ "Fantomas.FCS" ]

    // Analyzing a project costs roughly what type checking it costs, so the largest one decides how
    // long the whole run takes. Starting with it means it is never the one left waiting for a slot.
    let sourceSize (project: string) =
        Directory.EnumerateFiles(Path.GetDirectoryName(repositoryRoot </> project), "*.fs", SearchOption.AllDirectories)
        |> Seq.sumBy (fun file -> FileInfo(file).Length)

    XDocument.Load(repositoryRoot </> "fantomas.slnx").XPathSelectElements("//Project")
    |> Seq.map (fun project -> project.Attribute(XName.Get "Path").Value.Replace('\\', '/'))
    |> Seq.filter (fun path -> not (excluded.Contains(Path.GetFileNameWithoutExtension path)))
    |> Seq.sortByDescending sourceSize
    |> Seq.toList

/// One project to hand to the analyzers, and which of its files to look at.
///
/// `Files` holds absolute paths, because that is the only form `--include-files` matches: give it a
/// path relative to the repository root and it matches nothing, says nothing about it and reports a
/// clean project. An empty list asks for every file of the project.
type AnalysisTarget = { Project: string; Files: string list }

/// What to analyze for a set of changed files: every project that owns one, along with the files of
/// its own that changed. A project owns everything under its own folder, which is how every project
/// of this solution is laid out. The order is the one `projectsToAnalyze` puts them in.
///
/// Only compiled sources and project files count. A script, a document or a test data file is not
/// part of any compilation, so changing one leaves the analyzers with nothing new to say.
///
/// A changed project file asks for the whole project: what it compiles is no longer what it
/// compiled before, and there is no single source file that stands for that.
let targetsFor (files: string list) : AnalysisTarget list =
    let sources: string list = List.filter (hasExtension [ ".fs"; ".fsi" ]) files
    let projectFiles: string list = List.filter (hasExtension [ ".fsproj" ]) files

    projectsToAnalyze
    |> List.choose (fun (project: string) ->
        let folder: string = project.Substring(0, project.LastIndexOf '/' + 1)

        let owns (file: string) : bool =
            file.StartsWith(folder, StringComparison.Ordinal)

        if List.exists owns projectFiles then
            Some { Project = project; Files = [] }
        else
            match List.filter owns sources with
            | [] -> None
            | owned ->
                Some
                    { Project = project
                      Files = List.map (fun (file: string) -> repositoryRoot </> file) owned })

/// Where the analyzer project this repository owns is built to. It is deliberately outside the
/// solution and does not inherit the root `Directory.Build.props`, so this is an ordinary
/// `bin` folder rather than anything under `artifacts`.
///
/// `--analyzers-path` is handed this folder rather than `analyzers`, because the SDK searches
/// recursively for `*Analyzer*.dll` and would otherwise also find `Fantomas.Analyzers.Tests.dll`.
let localAnalyzerPath: string =
    repositoryRoot
    </> "analyzers"
    </> "Fantomas.Analyzers"
    </> "bin"
    </> "Release"
    </> "net8.0"

/// The analyzers are in the solution, so `Build` compiles and tests them along with everything
/// else. The `Analyze` pipelines do not depend on `Build` having run, so they build them again,
/// which is cheap and means editing a rule and rerunning the analysis is a single command.
let buildLocalAnalyzers: string =
    "dotnet build analyzers/Fantomas.Analyzers -c Release --tl"

/// Where the analyzers live on disk. The two packages are ordinary package references, so MSBuild
/// already knows the restored path of each and there is no second place to keep the version in
/// sync. The third is ours, and is built by the pipeline that is about to use it.
let analyzerPaths () : Async<string list> =
    async {
        if not (File.Exists(localAnalyzerPath </> "Fantomas.Analyzers.dll")) then
            failwith
                $"The local analyzers are not built. Expected an assembly in {localAnalyzerPath}.\nRun `dotnet build analyzers/Fantomas.Analyzers -c Release` first."

        let! result =
            Cli
                .Wrap("dotnet")
                .WithArguments(
                    "msbuild src/Fantomas/Fantomas.fsproj -getProperty:PkgIonide_Analyzers "
                    + "-getProperty:PkgG-Research_FSharp_Analyzers"
                )
                .WithWorkingDirectory(repositoryRoot)
                .WithValidation(CommandResultValidation.None)
                .ExecuteBufferedAsync()
                .Task
            |> Async.AwaitTask

        if result.ExitCode <> 0 then
            failwith $"Could not resolve the analyzer packages. Run `dotnet restore` first.\n{result.StandardError}"

        let properties = JsonValue.Parse(result.StandardOutput).GetProperty("Properties")

        return
            [ for property in properties.Properties() do
                  let name, value = property

                  match value.AsString() with
                  | "" -> failwith $"MSBuild has no value for {name}. Run `dotnet restore` first."
                  | path -> path </> "analyzers" </> "dotnet" </> "fs"

              localAnalyzerPath ]
    }

/// The number of results a single analyzer report holds, used to report what a project turned up
/// the moment it finishes.
let sarifResultCount (report: string) : int =
    if not (File.Exists report) then
        0
    else
        JsonValue.Parse(File.ReadAllText report).GetProperty("runs").AsArray()
        |> Array.sumBy (fun run ->
            match run.TryGetProperty "results" with
            | Some results -> results.AsArray().Length
            | None -> 0)

/// Folds the per-project reports into the one SARIF run that GitHub code scanning takes.
///
/// SARIF carries a run per tool invocation, but code scanning rejects a file holding several unless
/// each names its own category, and one project of this solution is not an analysis of its own. The
/// runs all come from the same tool, so their results concatenate into a single run. Every
/// invocation is kept, which is what records that a project was looked at even when it turned up
/// nothing.
///
/// The reports carry no rule metadata, only a `ruleId` per result, so there is no rule table to
/// renumber against. Should a later version of the analyzers SDK start writing one, this has to
/// merge that too.
/// The same record with one property replaced, leaving every other property where it was.
let withProperty (name: string) (value: JsonValue) (record: JsonValue) : JsonValue =
    JsonValue.Record
        [| for existing, current in record.Properties() ->
               if existing = name then
                   existing, value
               else
                   existing, current |]

/// A run's `tool.driver.rules`, which is empty when it has none.
let rulesOf (run: JsonValue) : JsonValue array =
    run.TryGetProperty "tool"
    |> Option.bind (fun (tool: JsonValue) -> tool.TryGetProperty "driver")
    |> Option.bind (fun (driver: JsonValue) -> driver.TryGetProperty "rules")
    |> Option.map (fun (rules: JsonValue) -> rules.AsArray())
    |> Option.defaultValue [||]

/// The same run carrying these rules instead.
let withRules (rules: JsonValue array) (run: JsonValue) : JsonValue =
    match run.TryGetProperty "tool" with
    | None -> run
    | Some tool ->
        match tool.TryGetProperty "driver" with
        | None -> run
        | Some driver ->
            let driver: JsonValue = withProperty "rules" (JsonValue.Array rules) driver
            withProperty "tool" (withProperty "driver" driver tool) run

let mergeSarifReports (reports: string list) (target: string) : unit =
    let documents =
        reports
        |> List.filter File.Exists
        |> List.map (fun report -> JsonValue.Parse(File.ReadAllText report))

    let runs =
        documents
        |> List.collect (fun document -> document.GetProperty("runs").AsArray() |> List.ofArray)

    match documents, runs with
    | firstDocument :: _, firstRun :: _ ->
        let concat (name: string) =
            runs
            |> List.collect (fun run ->
                match run.TryGetProperty name with
                | Some array -> List.ofArray (array.AsArray())
                | None -> [])
            |> Array.ofList
            |> JsonValue.Array

        // `ruleIndex` addresses `tool.driver.rules` by position within its own run, so merging the
        // runs means pointing every result at where its own rule ended up. Keeping the first run's
        // rules and every run's results, as this used to, left every run after the first pointing
        // into an array it was never numbered against.
        //
        // Identical entries collapse. The analyzers write one entry per finding rather than one per
        // rule, its `name` being that finding's message, so the same entry is written again for
        // every finding that reads the same: two bindings called `filename` with no annotation
        // produce the same id and the same message, in one project or in two. GitHub refuses to
        // ingest a document whose rules array holds a duplicate, and it is the merged document that
        // is uploaded.
        let rules, results =
            let merged: ResizeArray<JsonValue> = ResizeArray()

            let seen: Collections.Generic.Dictionary<string, int> =
                Collections.Generic.Dictionary()

            let indexOf (rule: JsonValue) : int =
                let key: string = rule.ToString()

                match seen.TryGetValue key with
                | true, index -> index
                | false, _ ->
                    let index: int = merged.Count
                    merged.Add rule
                    seen[key] <- index
                    index

            let results: JsonValue list =
                runs
                |> List.collect (fun (run: JsonValue) ->
                    // Every rule of the run is placed, whether a result points at it or not, so that
                    // this says the same as before about what the tool knows.
                    let placed: int array = Array.map indexOf (rulesOf run)

                    let repoint (result: JsonValue) : JsonValue =
                        match result.TryGetProperty "ruleIndex" with
                        | None -> result
                        | Some index ->
                            let original: int = index.AsInteger()

                            if original >= 0 && original < placed.Length then
                                withProperty "ruleIndex" (JsonValue.Number(decimal placed[original])) result
                            else
                                result

                    match run.TryGetProperty "results" with
                    | None -> []
                    | Some results -> results.AsArray() |> Array.map repoint |> List.ofArray)

            List.ofSeq merged, results

        let merged =
            JsonValue.Record
                [| "$schema", firstDocument.GetProperty("$schema")
                   "version", firstDocument.GetProperty("version")
                   "runs",
                   JsonValue.Array
                       [| JsonValue.Record
                              [| "tool", (withRules (Array.ofList rules) firstRun).GetProperty("tool")
                                 "columnKind", firstRun.GetProperty("columnKind")
                                 "results", JsonValue.Array(Array.ofList results)
                                 "invocations", concat "invocations" |] |] |]

        File.WriteAllText(target, merged.ToString())
    | _ -> failwith "The analyzers wrote no report to merge."

/// Runs the analyzers over the given targets, one process per project, several at a time.
///
/// A single process walking every project in turn takes minutes and says nothing until the last one
/// is done, which is a long time to stare at a blank terminal. Each project is instead analyzed on
/// its own, and its output is held back and printed in one piece as that project finishes, so
/// findings arrive while the run is still going and no two projects can interleave their lines.
///
/// A target that names files is analyzed for those files alone. The project is still loaded and
/// type checked, but a whole project is checked file by file, so looking at one file of
/// `Fantomas.Core.Tests` takes seconds where the whole project takes minutes.
///
/// Whatever is analyzed here is what `analysis.sarif` holds afterwards, so a run over a couple of
/// files replaces the report of an earlier run over the solution.
///
/// The local rules that report at error severity, and so fail a run when they fire.
///
/// `AnalyzeChanged` demotes these, because the run you do while working should report everything
/// and stop for nothing. `Analyze` leaves them alone, so CI is where they bite.
let localErrorRules: string list =
    [ "FANTOMAS-PIPEBACK-001"; "FANTOMAS-PRIVATE-001" ]

/// The local analyzers that are kept out of the full run.
///
/// Both report on debt that predates them, and a finding in `Analyze` becomes a code scanning alert
/// on the pull request whatever its severity. `AnalyzeChanged` still runs them, over the files you
/// touched, which is the scope the rules ask for. Drop this once the debt is gone.
let localAdvisoryAnalyzers: string list = [ "AnnotationAnalyzer"; "XmlDocAnalyzer" ]

/// The codes of those same rules, which is what a finding carries.
let localAdvisoryCodes: Set<string> =
    set [ "FANTOMAS-ANNOTATE-001"; "FANTOMAS-XMLDOC-001" ]

/// Decides whether a finding is worth showing, from its rule, its file and its line. `Analyze`
/// shows all of them; only `AnalyzeChanged` narrows.
type FindingFilter = string -> string -> int -> bool

let everyFinding: FindingFilter = fun _ _ _ -> true

/// Whether a finding is one this run set out to report.
///
/// Two questions, in order. **Is the file one the working tree changed?** If not the finding is
/// dropped whatever its rule, because `AnalyzeChanged` reports on the code in front of you and this
/// is not it. That test only started mattering once a changed `.fsproj` began asking for the whole
/// project: analysing every file of `Fantomas.Tests` to report on the two you added buries them
/// under the project's existing debt, and a run whose findings you have to hand-filter is a run that
/// tells you nothing.
///
/// **And, for the two advisory rules, is it on a line that changed?** A file is a much coarser scope
/// than those two ask for: one line changed in a file of several thousand otherwise surfaces every
/// unannotated binding in it, and the annotation rule explicitly says to leave alone the bindings you
/// had no reason to open. Every other rule reports anywhere in a file you edited, which is the scope
/// those rules do ask for.
///
/// A file git has never seen is new in its entirety, so everything in it is worth reporting.
let keepFinding (scopes: Map<string, ChangedLines>) : FindingFilter =
    fun (code: string) (path: string) (line: int) ->
        match scopeFor scopes path with
        | None -> false
        | Some WholeFile -> true
        | Some(Lines lines) -> not (localAdvisoryCodes.Contains code) || Set.contains line lines

/// Drops the advisory findings that sit on lines the working tree did not touch.
///
/// `AnalyzeChanged` scopes itself to the files you edited, which for these two rules is much
/// coarser than the rules ask for: one line changed in a file of several thousand surfaces every
/// unannotated binding in it, and the annotation rule explicitly says to leave the bindings you had
/// no reason to open alone. The other rules are left alone, because a finding from one of those is
/// worth seeing wherever it is.
///
/// Reads the tool's own output format. Anything it cannot parse is kept, so a change upstream makes
/// this stop narrowing rather than start hiding.
let narrowOutput (keep: FindingFilter) (output: string) : string =
    let finding: Text.RegularExpressions.Regex =
        Text.RegularExpressions.Regex(@"^(?<path>.+?)\((?<line>\d+),\d+\): \w+ (?<code>[A-Z][A-Z0-9-]*) :")

    output.Split('\n')
    |> Array.filter (fun (line: string) ->
        let m: Text.RegularExpressions.Match =
            finding.Match(line.TrimStart('\u001b').TrimStart())

        if not m.Success then
            true
        else
            keep m.Groups["code"].Value m.Groups["path"].Value (int m.Groups["line"].Value))
    |> String.concat "\n"

/// The same narrowing, over the report a project just wrote, so that `analysis.sarif` and what was
/// printed say the same thing.
let narrowReport (keep: FindingFilter) (report: string) : unit =
    if File.Exists report then
        let document: JsonValue = JsonValue.Parse(File.ReadAllText report)

        let keepResult (result: JsonValue) : bool =
            match result.TryGetProperty "ruleId" with
            | None -> true
            | Some ruleId ->
                let location: JsonValue =
                    result.GetProperty("locations").AsArray().[0].GetProperty("physicalLocation")

                let path: string =
                    location.GetProperty("artifactLocation").GetProperty("uri").AsString()

                let line: int = location.GetProperty("region").GetProperty("startLine").AsInteger()
                keep (ruleId.AsString()) path line

        // The tool writes one `tool.driver.rules` entry per finding, whose `name` is that finding's
        // own message rather than the rule's. Filtering `results` and leaving the rules alone
        // therefore leaves every dropped finding's message behind, and a report whose `results` is
        // empty while `rules` still spells out seventy findings reads as a contradiction, and is
        // one. So the rules no surviving result points at go too, and what is left is renumbered,
        // because `ruleIndex` addresses that array by position.
        let narrowRun (run: JsonValue) : JsonValue =
            match run.TryGetProperty "results" with
            | None -> run
            | Some results ->
                let kept: JsonValue array = Array.filter keepResult (results.AsArray())
                let rules: JsonValue array = rulesOf run

                let referenced: int array =
                    kept
                    |> Array.choose (fun (result: JsonValue) ->
                        result.TryGetProperty "ruleIndex"
                        |> Option.map (fun (index: JsonValue) -> index.AsInteger()))
                    |> Array.filter (fun (index: int) -> index >= 0 && index < rules.Length)
                    |> Array.distinct
                    |> Array.sort

                let renumbered: Map<int, int> =
                    referenced
                    |> Array.mapi (fun (position: int) (original: int) -> original, position)
                    |> Map.ofArray

                let repointed: JsonValue array =
                    kept
                    |> Array.map (fun (result: JsonValue) ->
                        match result.TryGetProperty "ruleIndex" with
                        | None -> result
                        | Some index ->
                            match Map.tryFind (index.AsInteger()) renumbered with
                            | None -> result
                            | Some position -> withProperty "ruleIndex" (JsonValue.Number(decimal position)) result)

                run
                |> withProperty "results" (JsonValue.Array repointed)
                |> withRules (Array.map (fun (index: int) -> rules[index]) referenced)

        let runs: JsonValue array =
            document.GetProperty("runs").AsArray() |> Array.map narrowRun

        let narrowed: JsonValue =
            JsonValue.Record
                [| for name, value in document.Properties() ->
                       if name = "runs" then
                           name, JsonValue.Array runs
                       else
                           name, value |]

        File.WriteAllText(report, narrowed.ToString())

/// Returns the highest exit code of the runs, so a project the analyzers could not process fails
/// the stage rather than passing for want of findings.
///
/// `extraArguments` is passed to every invocation, and is how the two pipelines differ.
let analyzeTargets (extraArguments: string list) (keep: FindingFilter) (targets: AnalysisTarget list) : Async<int> =
    async {
        let! analyzers = analyzerPaths ()

        if Directory.Exists analysisReportsDir then
            Directory.Delete(analysisReportsDir, true)

        Directory.CreateDirectory analysisReportsDir |> ignore

        let names =
            targets
            |> List.map (fun (target: AnalysisTarget) -> Path.GetFileNameWithoutExtension target.Project)
            |> String.concat ", "

        let count: string =
            match targets.Length with
            | 1 -> "1 project"
            | n -> $"{n} projects"

        printfn $"Analyzing {count}: {names}"

        let analyzeProject (target: AnalysisTarget) =
            async {
                let name = Path.GetFileNameWithoutExtension target.Project
                let report = analysisReportsDir </> $"{name}.sarif"
                let started = DateTime.UtcNow

                let arguments =
                    [ "fsharp-analyzers"
                      // The test SDK generates this entry point into the compilation from the
                      // package cache. It is part of what gets type checked but it is not ours.
                      "--exclude-files"
                      "**/Microsoft.NET.Test.Sdk.Program.fs"
                      for analyzer in analyzers do
                          "--analyzers-path"
                          analyzer
                      // One flag, then every file. Repeating the flag is an error, and the tool
                      // answers it by printing its help and finding nothing, which reads as a
                      // clean project.
                      match target.Files with
                      | [] -> ()
                      | files ->
                          "--include-files"
                          yield! files
                      "--code-root"
                      repositoryRoot
                      "--report"
                      report
                      yield! extraArguments
                      "--project"
                      repositoryRoot </> target.Project ]

                let! result =
                    Cli
                        .Wrap("dotnet")
                        .WithArguments(arguments)
                        .WithWorkingDirectory(repositoryRoot)
                        .WithValidation(CommandResultValidation.None)
                        .ExecuteBufferedAsync()
                        .Task
                    |> Async.AwaitTask

                narrowReport keep report

                let elapsed = DateTime.UtcNow - started
                let findings = sarifResultCount report

                // A non-zero exit is worth saying out loud. The tool exits non-zero both for a
                // finding at error severity and for a run that never happened, and a bare
                // "no findings" would read the same either way.
                let summary =
                    match result.ExitCode, findings with
                    | 0, 0 -> "no findings"
                    | 0, 1 -> "1 finding"
                    | 0, n -> $"{n} findings"
                    | code, 0 -> $"no findings, exit code {code}"
                    | code, n -> $"{n} findings, exit code {code}"

                let scope =
                    match target.Files with
                    | [] -> ""
                    | [ _ ] -> " (1 file)"
                    | files -> $" ({files.Length} files)"

                printfn $"\n=== {name}{scope}: {summary} in {elapsed.TotalSeconds:F1}s"
                printf "%s" (narrowOutput keep result.StandardOutput)
                eprintf "%s" result.StandardError

                return report, result.ExitCode
            }

        // Every analyzer process type checks a whole project, so a handful at a time is what keeps
        // the machine busy without the runs starving each other of memory.
        let! results = Async.Parallel(List.map analyzeProject targets, max 2 (Environment.ProcessorCount / 2))

        mergeSarifReports (results |> Array.map fst |> List.ofArray) (mergedAnalysisReport)

        return results |> Array.map snd |> Array.fold max 0
    }

/// Each of these takes a list of values after a single flag. Repeating the flag is an error.
let excludeLocalAdvisory: string list =
    "--exclude-analyzers" :: localAdvisoryAnalyzers
