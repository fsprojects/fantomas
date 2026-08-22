#!/usr/bin/env -S dotnet fsi

#r "nuget: Fun.Build, 1.1.16"
#r "nuget: CliWrap, 3.6.4"
#r "nuget: FSharp.Data, 6.3.0"
#r "nuget: Ionide.KeepAChangelog, 0.1.8"
#r "nuget: Humanizer.Core, 2.14.1"

open System
open System.IO
open Fun.Build
open CliWrap
open CliWrap.Buffered
open FSharp.Data
open System.Xml.Linq
open System.Xml.XPath
open Ionide.KeepAChangelog
open Ionide.KeepAChangelog.Domain
open SemVersion
open Humanizer

let (</>) a b = Path.Combine(a, b)

// Every path here is anchored to the script folder. A relative path is resolved against the
// working directory of the process, which is not necessarily the folder this script lives in.
let artifactsDir = __SOURCE_DIRECTORY__ </> "artifacts"
let binDir = artifactsDir </> "bin"
let packagesDir = artifactsDir </> "package" </> "release"
let analysisReportsDir = __SOURCE_DIRECTORY__ </> "analysisreports"
let coverageReportDir = __SOURCE_DIRECTORY__ </> "coveragereport"

/// Every test project, by name. Each writes its raw coverage beside its own project file.
let coverageProjects: string list =
    [ "Fantomas.Core.Tests"; "Fantomas.Tests"; "Fantomas.Client.Tests" ]

let coverageXmlFiles: string list =
    coverageProjects
    |> List.map (fun (name: string) -> __SOURCE_DIRECTORY__ </> "src" </> name </> "coverage.xml")

/// Run one test project under AltCover, measuring the one assembly it is there to exercise.
///
/// The filter is a negative lookahead: instrument that assembly and nothing else, which keeps the
/// generated Fantomas.FCS parser and the test assembly itself out of the report and makes the run
/// fast. It cannot name several assemblies at once, because AltCover reads `|` as the separator
/// between filters rather than as alternation, so each project is run with its own.
let coverageCommand (name: string) (assemblyPattern: string) : string =
    let project: string = __SOURCE_DIRECTORY__ </> "src" </> name </> $"{name}.fsproj"

    $"dotnet test {project} -c Release /p:AltCover=true "
    + $"\"/p:AltCoverAssemblyFilter=^(?!{assemblyPattern}$)\""

let benchmarkAssembly =
    binDir </> "Fantomas.Benchmarks" </> "release" </> "Fantomas.Benchmarks.dll"

let semanticVersioning =
    binDir </> "Fantomas" </> "release" </> "SemanticVersioning.dll"

let isDryRun =
    let args = fsi.CommandLineArgs
    Array.exists (fun arg -> arg = "--dry-run") args

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

let pushPackage nupkg =
    async {
        if isDryRun then
            printfn $"[DRY-RUN] Would push package: {nupkg}"
            return 0
        else
            let key = Environment.GetEnvironmentVariable("NUGET_KEY")
            let! result =
                Cli
                    .Wrap("dotnet")
                    .WithArguments(
                        $"nuget push \"{nupkg}\" --api-key \"{key}\" --source https://api.nuget.org/v3/index.json"
                    )
                    .ExecuteAsync()
                    .Task
                |> Async.AwaitTask
            return result.ExitCode
    }

pipeline "Build" {
    workingDir __SOURCE_DIRECTORY__
    stage "RestoreTools" { run "dotnet tool restore" }
    stage "Clean" { run (cleanFolders [| analysisReportsDir; artifactsDir |]) }
    stage "CheckFormat" { run "dotnet fantomas src docs scripts build.fsx --check" }
    stage "Build" { run "dotnet build -c Release --tl" }
    stage "UnitTests" { run "dotnet test -c Release --tl" }
    stage "Pack" { run "dotnet pack --no-restore -c Release --tl" }
    stage "Docs" {
        whenNot { platformOSX }
        envVars
            [| "DOTNET_ROLL_FORWARD_TO_PRERELEASE", "1"
               "DOTNET_ROLL_FORWARD", "LatestMajor" |]
        run
            $"dotnet fsdocs build --clean --properties Configuration=Release --fscoptions \" -r:{semanticVersioning}\" --eval --strict --nonpublic"
    }
    runIfOnlySpecified false
}

pipeline "Benchmark" {
    workingDir __SOURCE_DIRECTORY__
    stage "Prepare" { run "dotnet build -c Release src/Fantomas.Benchmarks --tl" }
    stage "Benchmark" { run $"dotnet \"{benchmarkAssembly}\"" }
    runIfOnlySpecified true
}

// Line and branch coverage for the three projects Fantomas ships, via AltCover's MSBuild
// integration. Every test project is run under AltCover, each measuring the one assembly it is
// there to exercise, and ReportGenerator merges the three results into a single report.
//
// So `Fantomas.Core`'s figure comes from `Fantomas.Core.Tests` alone, even though `Fantomas.Tests`
// exercises Core heavily through real formatting. Core is understated here rather than wrong.
//
// The filter is a negative lookahead naming the three assemblies to instrument. Everything else
// is left alone, which keeps the generated Fantomas.FCS parser and the test assemblies
// themselves out of the report. AltCover writes OpenCover XML, which is for tooling rather than
// reading, so ReportGenerator turns it into a browsable HTML report afterwards.
//
// A test that starts the fantomas process, as those in Fantomas.Tests/Integration do, adds
// nothing here, because the child process is not instrumented. That is the point rather than a
// flaw: what this measures is how much of the tool can be reached without starting one.
//
// Produces:
//   src/<project>/coverage.xml    raw OpenCover XML, one per test project
//   coveragereport/index.html     browsable report, per file and per line
pipeline "Coverage" {
    workingDir __SOURCE_DIRECTORY__
    stage "RestoreTools" { run "dotnet tool restore" }

    stage "Clean" {
        run (cleanFolders [| coverageReportDir |])
        // A stale coverage.xml from an earlier run would otherwise be merged into the report.
        run (fun _ ->
            async {
                for file in coverageXmlFiles do
                    if File.Exists file then
                        File.Delete file

                return 0
            })
    }

    stage "Coverage" {
        run (coverageCommand "Fantomas.Core.Tests" @"Fantomas\.Core")
        run (coverageCommand "Fantomas.Tests" "fantomas")
        run (coverageCommand "Fantomas.Client.Tests" @"Fantomas\.Client")
    }

    stage "Report" {
        run (
            $"dotnet reportgenerator -reports:{String.Join(';', coverageXmlFiles)} "
            + $"-targetdir:{coverageReportDir} -reporttypes:Html;TextSummary"
        )

        run (fun _ ->
            async {
                let summary = coverageReportDir </> "Summary.txt"
                let index = coverageReportDir </> "index.html"

                if File.Exists summary then
                    printfn "%s" (File.ReadAllText summary)

                printfn $"Browse the full report at {index}"
                return 0
            })
    }

    runIfOnlySpecified true
}

let runGitCommand (arguments: string) =
    async {
        let! result =
            Cli
                .Wrap("git")
                .WithArguments(arguments)
                .WithWorkingDirectory(__SOURCE_DIRECTORY__)
                .ExecuteBufferedAsync()
                .Task
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

pipeline "FormatChanged" {
    workingDir __SOURCE_DIRECTORY__
    stage "Format" {
        run (fun _ ->
            async {
                let! files = changedFiles ()
                let sources: string list =
                    List.filter (hasExtension [ ".fs"; ".fsx"; ".fsi" ]) files

                match sources with
                | [] ->
                    printfn "No changed F# files to format."
                    return 0
                | sources ->
                    let arguments: string =
                        sources
                        |> List.map (fun (source: string) -> $"\"{source}\"")
                        |> String.concat " "

                    return! runCmd "dotnet" $"fantomas {arguments}"
            })
    }
    runIfOnlySpecified true
}

pipeline "PushClient" {
    workingDir __SOURCE_DIRECTORY__
    stage "Pack" { run "dotnet pack ./src/Fantomas.Client -c Release --tl" }
    stage "Push" {
        run (fun _ ->
            async {
                return!
                    Directory.EnumerateFiles(packagesDir, "Fantomas.Client.*.nupkg", SearchOption.TopDirectoryOnly)
                    |> Seq.tryExactlyOne
                    |> Option.map pushPackage
                    |> Option.defaultValue (
                        async {
                            printfn "Fantomas.Client package was not found."
                            return -1
                        }
                    )
            })
    }
    runIfOnlySpecified true
}

pipeline "Docs" {
    workingDir __SOURCE_DIRECTORY__
    stage "Prepare" {
        run "dotnet tool restore"
        run "dotnet build -c Release src/Fantomas/Fantomas.fsproj"
    }
    stage "Watch" {
        envVars
            [| "DOTNET_ROLL_FORWARD_TO_PRERELEASE", "1"
               "DOTNET_ROLL_FORWARD", "LatestMajor" |]
        run
            $"dotnet fsdocs watch --properties Configuration=Release --fscoptions \" -r:{semanticVersioning}\" --eval --nonpublic"
    }
    runIfOnlySpecified true
}

pipeline "FormatAll" {
    workingDir __SOURCE_DIRECTORY__
    stage "Fantomas" { run "dotnet fantomas src docs scripts build.fsx" }
    runIfOnlySpecified true
}

pipeline "EnsureRepoConfig" {
    workingDir __SOURCE_DIRECTORY__
    stage "Git" { run "git config core.hooksPath .githooks" }
    runIfOnlySpecified true
}

let deps = __SOURCE_DIRECTORY__ </> ".deps"

let fsharpCompilerHash =
    let xDoc = XElement.Load(__SOURCE_DIRECTORY__ </> "Directory.Build.props")
    xDoc.XPathSelectElements("//FCSCommitHash") |> Seq.head |> (fun xe -> xe.Value)

let updateFileRaw (file: FileInfo) =
    let lines = File.ReadAllLines file.FullName
    let updatedLines =
        lines
        |> Array.map (fun line ->
            if line.StartsWith("namespace FSharp.Build") then
                line.Replace("namespace FSharp.Build", "namespace Fantomas.FCS.Build")
            elif line.Contains("FSharp.Compiler") then
                line.Replace("FSharp.Compiler", "Fantomas.FCS")
            elif line.Contains("[<TailCall>]") then
                line.Replace("[<TailCall>]", "[<Microsoft.FSharp.Core.TailCall>]")
            else
                line)
    File.WriteAllLines(file.FullName, updatedLines)

let downloadCompilerFile commitHash relativePath =
    async {
        let file = FileInfo(deps </> commitHash </> relativePath)
        if file.Exists && file.Length <> 0 then
            return ()
        else
            file.Directory.Create()
            let fs = file.Create()
            let fileName = Path.GetFileName(relativePath)
            let url =
                $"https://raw.githubusercontent.com/dotnet/fsharp/{commitHash}/{relativePath}"
            let! response =
                Http.AsyncRequestStream(
                    url,
                    headers = [| "Content-Disposition", $"attachment; filename=\"{fileName}\"" |]
                )
            if response.StatusCode <> 200 then
                printfn $"Could not download %s{relativePath}"
            do! Async.AwaitTask(response.ResponseStream.CopyToAsync(fs))
            fs.Close()

            updateFileRaw file
    }

pipeline "Init" {
    workingDir __SOURCE_DIRECTORY__
    stage "Download FCS files" {
        run (fun _ ->
            [|
               // Not a compiler source. This is the MSBuild task that turns FSComp.txt into the SR
               // module. Since dotnet/fsharp#20097 the generated diagnostic accessors return RichText
               // instead of string, and the task shipped in the .NET SDK cannot generate those yet.
               "src/FSharp.Build/FSharpEmbedResourceText.fs"
               "src/Compiler/FSComp.txt"
               "src/Compiler/FSStrings.resx"
               "src/Compiler/Utilities/NullHelpers.fs"
               "src/Compiler/Utilities/Activity.fsi"
               "src/Compiler/Utilities/Activity.fs"
               "src/Compiler/Utilities/Caches.fsi"
               "src/Compiler/Utilities/Caches.fs"
               "src/Compiler/Utilities/sformat.fsi"
               "src/Compiler/Utilities/sformat.fs"
               "src/Compiler/Utilities/sr.fsi"
               "src/Compiler/Utilities/sr.fs"
               "src/Compiler/Facilities/RichText.fsi"
               "src/Compiler/Facilities/RichText.fs"
               "src/Compiler/Utilities/ResizeArray.fsi"
               "src/Compiler/Utilities/ResizeArray.fs"
               "src/Compiler/Utilities/HashMultiMap.fsi"
               "src/Compiler/Utilities/HashMultiMap.fs"
               "src/Compiler/Utilities/ReadOnlySpan.fsi"
               "src/Compiler/Utilities/ReadOnlySpan.fs"
               "src/Compiler/Utilities/TaggedCollections.fsi"
               "src/Compiler/Utilities/TaggedCollections.fs"
               "src/Compiler/Utilities/illib.fsi"
               "src/Compiler/Utilities/illib.fs"
               "src/Compiler/Utilities/Cancellable.fsi"
               "src/Compiler/Utilities/Cancellable.fs"
               "src/Compiler/Utilities/FileSystem.fsi"
               "src/Compiler/Utilities/FileSystem.fs"
               "src/Compiler/Utilities/ildiag.fsi"
               "src/Compiler/Utilities/ildiag.fs"
               "src/Compiler/Utilities/zmap.fsi"
               "src/Compiler/Utilities/zmap.fs"
               "src/Compiler/Utilities/zset.fsi"
               "src/Compiler/Utilities/zset.fs"
               "src/Compiler/Utilities/XmlAdapters.fsi"
               "src/Compiler/Utilities/XmlAdapters.fs"
               "src/Compiler/Utilities/InternalCollections.fsi"
               "src/Compiler/Utilities/InternalCollections.fs"
               "src/Compiler/Utilities/lib.fsi"
               "src/Compiler/Utilities/lib.fs"
               "src/Compiler/Utilities/PathMap.fsi"
               "src/Compiler/Utilities/PathMap.fs"
               "src/Compiler/Utilities/range.fsi"
               "src/Compiler/Utilities/range.fs"
               "src/Compiler/Facilities/LanguageFeatures.fsi"
               "src/Compiler/Facilities/LanguageFeatures.fs"
               "src/Compiler/Facilities/DiagnosticOptions.fsi"
               "src/Compiler/Facilities/DiagnosticOptions.fs"
               "src/Compiler/Facilities/DiagnosticsLogger.fsi"
               "src/Compiler/Facilities/DiagnosticsLogger.fs"
               "src/Compiler/Facilities/Hashing.fsi"
               "src/Compiler/Facilities/Hashing.fs"
               "src/Compiler/Facilities/prim-lexing.fsi"
               "src/Compiler/Facilities/prim-lexing.fs"
               "src/Compiler/Facilities/prim-parsing.fsi"
               "src/Compiler/Facilities/prim-parsing.fs"
               "src/Compiler/AbstractIL/illex.fsl"
               "src/Compiler/AbstractIL/ilpars.fsy"
               "src/Compiler/AbstractIL/il.fsi"
               "src/Compiler/AbstractIL/il.fs"
               "src/Compiler/AbstractIL/ilascii.fsi"
               "src/Compiler/AbstractIL/ilascii.fs"
               "src/Compiler/SyntaxTree/PrettyNaming.fsi"
               "src/Compiler/SyntaxTree/PrettyNaming.fs"
               "src/Compiler/pplex.fsl"
               "src/Compiler/pppars.fsy"
               "src/Compiler/lex.fsl"
               "src/Compiler/pars.fsy"
               "src/Compiler/SyntaxTree/UnicodeLexing.fsi"
               "src/Compiler/SyntaxTree/UnicodeLexing.fs"
               "src/Compiler/SyntaxTree/XmlDocIncludeExpander.fsi"
               "src/Compiler/SyntaxTree/XmlDocIncludeExpander.fs"
               "src/Compiler/SyntaxTree/XmlDoc.fsi"
               "src/Compiler/SyntaxTree/XmlDoc.fs"
               "src/Compiler/SyntaxTree/SyntaxTrivia.fsi"
               "src/Compiler/SyntaxTree/SyntaxTrivia.fs"
               "src/Compiler/SyntaxTree/SyntaxTree.fsi"
               "src/Compiler/SyntaxTree/SyntaxTree.fs"
               "src/Compiler/SyntaxTree/SyntaxTreeOps.fsi"
               "src/Compiler/SyntaxTree/SyntaxTreeOps.fs"
               "src/Compiler/SyntaxTree/WarnScopes.fsi"
               "src/Compiler/SyntaxTree/WarnScopes.fs"
               "src/Compiler/SyntaxTree/LexerStore.fsi"
               "src/Compiler/SyntaxTree/LexerStore.fs"
               "src/Compiler/SyntaxTree/ParseHelpers.fsi"
               "src/Compiler/SyntaxTree/ParseHelpers.fs"
               "src/Compiler/SyntaxTree/LexHelpers.fsi"
               "src/Compiler/SyntaxTree/LexHelpers.fs"
               "src/Compiler/SyntaxTree/LexFilter.fsi"
               "src/Compiler/SyntaxTree/LexFilter.fs" |]
            |> Array.map (downloadCompilerFile fsharpCompilerHash)
            |> Async.Parallel
            |> Async.Ignore)
    }
    runIfOnlySpecified true
}

type GithubRelease =
    {
        Version: string
        Title: string
        Date: DateTime
        /// None when GitHub has no release for this version: it is not created yet, or the
        /// version went to NuGet by hand the way 7.0.6 did.
        PublishedDate: string option
        Draft: string
    }

let formatVersion (v: SemanticVersion) : string =
    if String.IsNullOrEmpty v.Prerelease then
        $"{v.Major}.{v.Minor}.{v.Patch}"
    else
        $"{v.Major}.{v.Minor}.{v.Patch}-{v.Prerelease}"

/// Releases are ordered on their version and not on their date. A hotfix for an older major is
/// released from its own branch, so it can enter the changelog with a date that is newer than
/// the entry main is about to release: 7.0.6 is dated after 8.0.0-alpha-013.
/// SemanticVersion itself does not support the comparison constraint, hence the tuple.
let versionSortKey (v: SemanticVersion) : int * int * int * int * string =
    let prerelease = if isNull v.Prerelease then String.Empty else v.Prerelease

    v.Major.GetValueOrDefault(),
    v.Minor.GetValueOrDefault(),
    v.Patch.GetValueOrDefault(),
    // a stable release comes after the prereleases that led up to it
    (if prerelease = String.Empty then 1 else 0),
    prerelease

/// The date the GitHub release for this version was published.
/// None when GitHub has no release for it, which is what happens for a version that was pushed
/// to NuGet by hand, like 7.0.6.
let getPublishedDate (version: string) : string option =
    let prefixedVersion = $"v{version}"
    printfn $"Checking if release {prefixedVersion} already exists on GitHub..."

    let cmdResult =
        Cli
            .Wrap("gh")
            .WithArguments($"release view {prefixedVersion} --json publishedAt -t \"{{{{.publishedAt}}}}\"")
            .WithValidation(CommandResultValidation.None)
            .ExecuteBufferedAsync()
            .Task.Result

    if cmdResult.ExitCode <> 0 then
        printfn $"Release {prefixedVersion} does not exist yet"
        None
    else
        let output = cmdResult.StandardOutput.Trim()
        let lastIdx = output.LastIndexOf("Z", StringComparison.Ordinal)
        let dateStr = output.Substring(0, lastIdx)
        printfn $"Release {prefixedVersion} already exists, published at: {dateStr}"
        Some dateStr

let mkGithubRelease (v: SemanticVersion, d: DateTime, cd: ChangelogData option) : GithubRelease =
    match cd with
    | None -> failwith "Each Fantomas release is expected to have at least one section."
    | Some cd ->
        let version = formatVersion v

        printfn $"Parsing release version: {version} (prerelease: {not (String.IsNullOrEmpty v.Prerelease)})"

        let title =
            let month = d.ToString("MMMM")
            let day = d.Day.Ordinalize()
            $"{month} {day} Release"

        let publishDate = getPublishedDate version

        let sections =
            [ "Added", cd.Added
              "Changed", cd.Changed
              "Fixed", cd.Fixed
              "Deprecated", cd.Deprecated
              "Removed", cd.Removed
              "Security", cd.Security
              yield! (Map.toList cd.Custom) ]
            |> List.choose (fun (header, lines) ->
                if lines.IsEmpty then
                    None
                else
                    lines
                    |> List.map (fun line -> line.TrimStart())
                    |> String.concat "\n"
                    |> sprintf "### %s\n%s" header
                    |> Some)
            |> String.concat "\n\n"

        let draft =
            $"""# {version}

{sections}"""

        { Version = version
          Title = title
          Date = d
          PublishedDate = publishDate
          Draft = draft }

let getReleaseNotes (currentRelease: GithubRelease) (lastPublishedDate: string option) : string =
    let date =
        match lastPublishedDate with
        | Some d ->
            printfn $"Using last release published date for author attribution: {d}"
            d
        | None ->
            // Query GitHub for the most recent published release
            printfn "No earlier changelog entry is on GitHub, querying GitHub for most recent release..."
            let ghReleaseResult =
                Cli
                    .Wrap("gh")
                    .WithArguments("release list --limit 1 --json createdAt")
                    .WithValidation(CommandResultValidation.None)
                    .ExecuteBufferedAsync()
                    .Task.Result

            if
                ghReleaseResult.ExitCode = 0
                && not (String.IsNullOrWhiteSpace(ghReleaseResult.StandardOutput.Trim()))
            then
                let jsonOutput = ghReleaseResult.StandardOutput.Trim()
                let jsonValue = FSharp.Data.JsonValue.Parse(jsonOutput)
                let releases = jsonValue.AsArray()
                if releases.Length > 0 then
                    match releases.[0].TryGetProperty("createdAt") with
                    | Some createdAtJson ->
                        let createdAt = createdAtJson.AsString()
                        // Parse ISO 8601 date and convert back to string format for the query
                        let dateTime =
                            DateTime
                                .Parse(createdAt, null, System.Globalization.DateTimeStyles.RoundtripKind)
                                .ToUniversalTime()
                        let ghDate = dateTime.ToString("yyyy-MM-ddTHH:mm:ss")
                        printfn $"Using most recent GitHub release date for author attribution: {ghDate}"
                        ghDate
                    | None ->
                        let fallbackDate = DateTime.UtcNow.ToString("yyyy-MM-dd")
                        printfn $"GitHub release missing createdAt, using current date: {fallbackDate}"
                        fallbackDate
                else
                    let fallbackDate = DateTime.UtcNow.ToString("yyyy-MM-dd")
                    printfn $"No GitHub releases found, using current date: {fallbackDate}"
                    fallbackDate
            else
                let fallbackDate = DateTime.UtcNow.ToString("yyyy-MM-dd")
                printfn $"Could not query GitHub releases, using current date: {fallbackDate}"
                fallbackDate

    printfn $"Querying PRs closed after {date} for author attribution..."

    let authorMsg =
        let queryResult =
            Cli
                .Wrap("gh")
                .WithArguments($"pr list -S \"state:closed base:main closed:>{date}\" --json commits,mergedAt")
                .WithValidation(CommandResultValidation.None)
                .ExecuteBufferedAsync()
                .Task.Result

        if queryResult.ExitCode <> 0 then
            printfn $"Warning: Failed to query PRs for author attribution (exit code: {queryResult.ExitCode})"
            String.Empty
        else
            let jsonOutput = queryResult.StandardOutput.Trim()

            // Parse JSON to filter by mergedAt timestamp
            let jsonValue = FSharp.Data.JsonValue.Parse(jsonOutput)
            let prs = jsonValue.AsArray()

            // Parse the date as ISO 8601 format (GitHub always returns dates in this format: "2025-08-02T10:25:30Z")
            let cutoffTimestamp =
                DateTime.Parse(date, null, System.Globalization.DateTimeStyles.RoundtripKind).ToUniversalTime()

            printfn $"Filtering PRs merged after: {cutoffTimestamp:O}"

            let authors =
                prs
                |> Array.collect (fun (pr: FSharp.Data.JsonValue) ->
                    let mergedAtOpt =
                        match pr.TryGetProperty("mergedAt") with
                        | Some mergedAtJson ->
                            let mergedAtStr = mergedAtJson.AsString()
                            match
                                DateTime.TryParse(mergedAtStr, null, System.Globalization.DateTimeStyles.RoundtripKind)
                            with
                            | true, dt -> Some(dt.ToUniversalTime())
                            | false, _ -> None
                        | None -> None

                    match mergedAtOpt with
                    | Some mergedAt when mergedAt > cutoffTimestamp ->
                        match pr.TryGetProperty("commits") with
                        | Some commitsJson ->
                            let commits = commitsJson.AsArray()
                            commits
                            |> Array.collect (fun (commit: FSharp.Data.JsonValue) ->
                                match commit.TryGetProperty("authors") with
                                | Some authorsJson ->
                                    let commitAuthors = authorsJson.AsArray()
                                    commitAuthors
                                    |> Array.choose (fun (author: FSharp.Data.JsonValue) ->
                                        match author.TryGetProperty("login") with
                                        | Some loginJson ->
                                            let login = loginJson.AsString()
                                            // Filter out bots
                                            if login.EndsWith("[bot]", StringComparison.Ordinal) then
                                                None
                                            else
                                                Some(login)
                                        | None -> None)
                                | None -> [||])
                        | None -> [||]
                    | _ -> [||])
                |> Array.distinct
                |> Array.sort

            printfn $"Found {authors.Length} contributors for this release"

            if authors.Length = 0 then
                String.Empty
            elif authors.Length = 1 then
                $"Special thanks to @%s{authors.[0]}!"
            else
                let lastAuthor = Array.last authors
                let otherAuthors =
                    if authors.Length = 2 then
                        $"@{authors.[0]}"
                    else
                        authors
                        |> Array.take (authors.Length - 1)
                        |> Array.map (sprintf "@%s")
                        |> String.concat ", "
                $"Special thanks to %s{otherAuthors} and @%s{lastAuthor}!"

    $"""{currentRelease.Draft}

{authorMsg}

[https://www.nuget.org/packages/fantomas/{currentRelease.Version}](https://www.nuget.org/packages/fantomas/{currentRelease.Version})
    """

let getCurrentReleaseAndLastPublishedDate () : GithubRelease * string option =
    printfn "Parsing CHANGELOG.md to find current and last release..."
    let changelog = FileInfo(__SOURCE_DIRECTORY__ </> "CHANGELOG.md")

    let changeLogResult =
        match Parser.parseChangeLog changelog with
        | Error error -> failwithf "Failed to parse changelog: %A" error
        | Ok result ->
            printfn $"Found {result.Releases.Length} releases in changelog"
            result

    let releases =
        changeLogResult.Releases
        |> List.sortByDescending (fun (v, _, _) -> versionSortKey v)

    match releases with
    | [] -> failwith "Could not find any release in CHANGELOG.md"
    | current :: earlierReleases ->
        let currentRelease = mkGithubRelease current
        printfn $"Current release: {currentRelease.Version}"

        // The release below the current one does not have to exist on GitHub: 7.0.6 went to
        // NuGet by hand from the v7.0.6 branch and never got a GitHub release. Walk down the
        // recent entries until GitHub knows one, its publish date is what the contributor
        // query is based on. Anything older than that is out of date anyway, getReleaseNotes
        // then falls back to the most recent release GitHub reports.
        let lastPublishedRelease =
            earlierReleases
            |> List.truncate 5
            |> List.tryPick (fun (v, _, _) ->
                let version = formatVersion v
                getPublishedDate version |> Option.map (fun date -> version, date))

        match lastPublishedRelease with
        | Some(version, date) -> printfn $"Last release on GitHub: {version}, published at {date}"
        | None -> printfn "None of the recent changelog entries has a GitHub release"

        currentRelease, Option.map snd lastPublishedRelease

pipeline "Release" {
    workingDir __SOURCE_DIRECTORY__
    stage "Build" { run "dotnet build -c Release" }
    stage "UnitTests" { run "dotnet test -c Release" }
    stage "Pack" { run "dotnet pack -c Release" }
    stage "Release" {
        run (fun _ ->
            async {
                if isDryRun then
                    printfn "[DRY-RUN] Starting release pipeline in dry-run mode"
                else
                    printfn "Starting release pipeline"

                let currentRelease, lastPublishedDate = getCurrentReleaseAndLastPublishedDate ()

                if Option.isSome currentRelease.PublishedDate then
                    printfn $"Release {currentRelease.Version} already exists on GitHub. Skipping release process."
                    return 0
                else
                    printfn $"Release {currentRelease.Version} does not exist yet. Proceeding with release process."

                    // Determine if this is a prerelease
                    let isPrerelease = currentRelease.Version.Contains("-")
                    if isPrerelease then
                        printfn $"Detected prerelease version: {currentRelease.Version}"

                    // Push packages to NuGet
                    let nugetPackages =
                        Directory.EnumerateFiles(packagesDir, "*.nupkg", SearchOption.TopDirectoryOnly)
                        |> Seq.filter (fun nupkg -> not (nupkg.Contains("Fantomas.Client")))
                        |> Seq.toArray

                    printfn $"Found {nugetPackages.Length} packages to push to NuGet:"
                    nugetPackages |> Array.iter (fun pkg -> printfn $"  - {Path.GetFileName(pkg)}")

                    let! nugetExitCodes = nugetPackages |> Array.map pushPackage |> Async.Sequential

                    let nugetSuccess = nugetExitCodes |> Array.forall (fun code -> code = 0)
                    if nugetSuccess then
                        printfn "All NuGet packages pushed successfully"
                    else
                        let exitCodesStr = nugetExitCodes |> Array.map string |> String.concat ", "
                        printfn $"Warning: Some NuGet packages failed to push. Exit codes: {exitCodesStr}"

                    let notes = getReleaseNotes currentRelease lastPublishedDate
                    printfn "Release notes that will be used:"
                    printfn "---"
                    printfn "%s" notes
                    printfn "---"
                    let noteFile = Path.GetTempFileName()
                    File.WriteAllText(noteFile, notes)
                    let files = nugetPackages |> Array.map (sprintf "\"%s\"") |> String.concat " "

                    // We create a draft release for minor and majors. Those that requires a manual publish.
                    // This is to allow us to add additional release notes when it makes sense.
                    // Extract patch version from currentRelease.Version (handle prerelease format)
                    let versionParts = currentRelease.Version.Split('-')
                    let mainVersion = versionParts.[0]
                    let patchVersion =
                        let parts = mainVersion.Split('.')
                        if parts.Length >= 3 then
                            match Int32.TryParse(parts.[2]) with
                            | true, p -> p
                            | _ -> 0
                        else
                            0

                    let isRevision = patchVersion <> 0
                    // Draft only for stable minor/major releases (patch = 0 and not prerelease)
                    let isDraftFlag =
                        if isRevision || isPrerelease then
                            String.Empty
                        else
                            "--draft"
                    let prereleaseFlag = if isPrerelease then "--prerelease" else String.Empty

                    let releaseType =
                        if isPrerelease then "prerelease (published)"
                        elif isRevision then "revision (published)"
                        else "minor/major (draft)"
                    printfn $"Release type: {releaseType}"
                    if isPrerelease then
                        printfn "This is a prerelease version"

                    let releaseCommand =
                        $"release create v{currentRelease.Version} {files} {isDraftFlag} {prereleaseFlag} --title \"{currentRelease.Title}\" --notes-file \"{noteFile}\""

                    let! draftExitCode =
                        if isDryRun then
                            printfn $"[DRY-RUN] Would execute: gh {releaseCommand}"
                            async { return 0 }
                        else
                            printfn $"Creating GitHub release: v{currentRelease.Version}"
                            async {
                                let! result =
                                    Cli
                                        .Wrap("gh")
                                        .WithArguments(releaseCommand)
                                        .WithValidation(CommandResultValidation.None)
                                        .ExecuteAsync()
                                        .Task
                                    |> Async.AwaitTask
                                return result.ExitCode
                            }

                    if File.Exists noteFile then
                        File.Delete(noteFile)

                    if draftExitCode = 0 then
                        printfn $"Successfully created GitHub release: v{currentRelease.Version}"
                    else
                        printfn $"Warning: GitHub release creation returned exit code: {draftExitCode}"

                    return Seq.max [| yield! nugetExitCodes; yield draftExitCode |]
            })
    }
    runIfOnlySpecified true
}

pipeline "PublishAlpha" {
    workingDir __SOURCE_DIRECTORY__
    stage "Clean" { run (cleanFolders [| analysisReportsDir; artifactsDir |]) }
    stage "Build" { run "dotnet build -c Release --tl" }
    stage "Pack" { run "dotnet pack --no-restore -c Release --tl" }
    stage "Publish" {
        run (fun ctx ->
            async {
                let nugetPackages =
                    Directory.EnumerateFiles(packagesDir, "*.nupkg", SearchOption.TopDirectoryOnly)
                    |> Seq.filter (fun nupkg -> not (nupkg.Contains("Fantomas.Client")))
                    |> Seq.toArray

                let! nugetExitCodes = nugetPackages |> Array.map pushPackage |> Async.Sequential

                return Seq.sum nugetExitCodes
            })
    }
    runIfOnlySpecified true
}

/// The projects the analyzers run over: every project in the solution, minus the ones whose source
/// is not ours to change. Fantomas.FCS is generated from the vendored compiler sources, and
/// Fantomas.FCS.BuildTasks compiles a single vendored compiler file, so a finding in either is
/// something to report upstream rather than something to fix here. Reading the solution rather than
/// globbing keeps build tooling out on its own: a project only gets analyzed once it is a real part
/// of the product.
let projectsToAnalyze: string list =
    let excluded = set [ "Fantomas.FCS" ]

    // Analyzing a project costs roughly what type checking it costs, so the largest one decides how
    // long the whole run takes. Starting with it means it is never the one left waiting for a slot.
    let sourceSize (project: string) =
        Directory.EnumerateFiles(
            Path.GetDirectoryName(__SOURCE_DIRECTORY__ </> project),
            "*.fs",
            SearchOption.AllDirectories
        )
        |> Seq.sumBy (fun file -> FileInfo(file).Length)

    XDocument.Load(__SOURCE_DIRECTORY__ </> "fantomas.slnx").XPathSelectElements("//Project")
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
                      Files = List.map (fun (file: string) -> __SOURCE_DIRECTORY__ </> file) owned })

/// Where the analyzers live on disk. Both are ordinary package references, so MSBuild already knows
/// the restored path of each and there is no second place to keep the version in sync.
let analyzerPaths () : Async<string list> =
    async {
        let! result =
            Cli
                .Wrap("dotnet")
                .WithArguments(
                    "msbuild src/Fantomas/Fantomas.fsproj -getProperty:PkgIonide_Analyzers "
                    + "-getProperty:PkgG-Research_FSharp_Analyzers"
                )
                .WithWorkingDirectory(__SOURCE_DIRECTORY__)
                .WithValidation(CommandResultValidation.None)
                .ExecuteBufferedAsync()
                .Task
            |> Async.AwaitTask

        if result.ExitCode <> 0 then
            failwith $"Could not resolve the analyzer packages. Run `dotnet restore` first.\n{result.StandardError}"

        let properties = JsonValue.Parse(result.StandardOutput).GetProperty("Properties")

        return
            [ for property in properties.Properties() ->
                  let name, value = property

                  match value.AsString() with
                  | "" -> failwith $"MSBuild has no value for {name}. Run `dotnet restore` first."
                  | path -> path </> "analyzers" </> "dotnet" </> "fs" ]
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

        let merged =
            JsonValue.Record
                [| "$schema", firstDocument.GetProperty("$schema")
                   "version", firstDocument.GetProperty("version")
                   "runs",
                   JsonValue.Array
                       [| JsonValue.Record
                              [| "tool", firstRun.GetProperty("tool")
                                 "columnKind", firstRun.GetProperty("columnKind")
                                 "results", concat "results"
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
/// Returns the highest exit code of the runs, so a project the analyzers could not process fails
/// the stage rather than passing for want of findings.
let analyzeTargets (targets: AnalysisTarget list) : Async<int> =
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
                      for analyzer in analyzers do
                          "--analyzers-path"
                          analyzer
                      for file in target.Files do
                          "--include-files"
                          file
                      "--code-root"
                      __SOURCE_DIRECTORY__
                      "--report"
                      report
                      "--project"
                      __SOURCE_DIRECTORY__ </> target.Project ]

                let! result =
                    Cli
                        .Wrap("dotnet")
                        .WithArguments(arguments)
                        .WithWorkingDirectory(__SOURCE_DIRECTORY__)
                        .WithValidation(CommandResultValidation.None)
                        .ExecuteBufferedAsync()
                        .Task
                    |> Async.AwaitTask

                let elapsed = DateTime.UtcNow - started
                let findings = sarifResultCount report

                let summary =
                    match findings with
                    | 0 -> "no findings"
                    | 1 -> "1 finding"
                    | n -> $"{n} findings"

                let scope =
                    match target.Files with
                    | [] -> ""
                    | [ _ ] -> " (1 file)"
                    | files -> $" ({files.Length} files)"

                printfn $"\n=== {name}{scope}: {summary} in {elapsed.TotalSeconds:F1}s"
                printf "%s" result.StandardOutput
                eprintf "%s" result.StandardError

                return report, result.ExitCode
            }

        // Every analyzer process type checks a whole project, so a handful at a time is what keeps
        // the machine busy without the runs starving each other of memory.
        let! results = Async.Parallel(List.map analyzeProject targets, max 2 (Environment.ProcessorCount / 2))

        mergeSarifReports (results |> Array.map fst |> List.ofArray) (__SOURCE_DIRECTORY__ </> "analysis.sarif")

        return results |> Array.map snd |> Array.fold max 0
    }

pipeline "Analyze" {
    workingDir __SOURCE_DIRECTORY__
    stage "RestoreTools" { run "dotnet tool restore" }
    stage "RestoreSolution" { run "dotnet restore --tl" }
    stage "Analyze" {
        run (fun _ ->
            projectsToAnalyze
            |> List.map (fun (project: string) -> { Project = project; Files = [] })
            |> analyzeTargets)
    }
    runIfOnlySpecified true
}

/// The same analyzers, over the files the working tree touches.
///
/// A project is only loaded when it owns a changed file, and is then analyzed for that file alone,
/// which is the difference between minutes and seconds on the test projects. What this cannot see
/// is a finding a change causes in a file other than the ones you edited, which is what the full
/// `Analyze` pipeline is still for before opening a pull request.
pipeline "AnalyzeChanged" {
    workingDir __SOURCE_DIRECTORY__
    stage "RestoreTools" { run "dotnet tool restore" }
    stage "RestoreSolution" { run "dotnet restore --tl" }

    stage "Analyze" {
        run (fun _ ->
            async {
                let! files = changedFiles ()

                match targetsFor files with
                | [] ->
                    printfn "No changed file belongs to a project that is analyzed."
                    return 0
                | targets -> return! analyzeTargets targets
            })
    }

    runIfOnlySpecified true
}

tryPrintPipelineCommandHelp ()
