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

let isDryRun =
    let args = fsi.CommandLineArgs
    Array.exists (fun arg -> arg = "--dry-run") args

let cleanFolders (input: string seq) =
    async {
        input
        |> Seq.iter (fun dir ->
            if Directory.Exists(dir) then
                Directory.Delete(dir, true))
    }

let benchmarkAssembly =
    "artifacts/bin/Fantomas.Benchmarks/release/Fantomas.Benchmarks.dll"

let semanticVersioning =
    __SOURCE_DIRECTORY__
    </> "artifacts"
    </> "bin"
    </> "Fantomas"
    </> "release"
    </> "SemanticVersioning.dll"

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
                    .WithArguments($"nuget push {nupkg} --api-key {key} --source https://api.nuget.org/v3/index.json")
                    .ExecuteAsync()
                    .Task
                |> Async.AwaitTask
            return result.ExitCode
    }

let analysisReportsDir = "analysisreports"

pipeline "Build" {
    workingDir __SOURCE_DIRECTORY__
    stage "RestoreTools" { run "dotnet tool restore" }
    stage "Clean" { run (cleanFolders [| analysisReportsDir; "artifacts" |]) }
    stage "CheckFormat" { run "dotnet fantomas src docs build.fsx --check" }
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
    stage "Benchmark" { run $"dotnet {benchmarkAssembly}" }
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

pipeline "FormatChanged" {
    workingDir __SOURCE_DIRECTORY__
    stage "Format" {
        run (fun _ ->
            async {
                let! exitCode, stdout, _stdErr = runGitCommand "status --porcelain"
                if exitCode <> 0 then
                    return exitCode
                else
                    let files =
                        stdout.Split('\n')
                        |> Array.choose (fun line ->
                            let line = line.Trim()
                            if
                                (line.StartsWith("AM", StringComparison.Ordinal)
                                 || line.StartsWith("M", StringComparison.Ordinal))
                                && (line.EndsWith(".fs", StringComparison.Ordinal)
                                    || line.EndsWith(".fsx", StringComparison.Ordinal)
                                    || line.EndsWith(".fsi", StringComparison.Ordinal))
                            then
                                Some(line.Replace("AM ", "").Replace("MM ", "").Replace("M ", ""))
                            else
                                None)
                        |> String.concat " "
                    return! runCmd "dotnet" $"fantomas {files}"
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
                    Directory.EnumerateFiles(
                        "artifacts/package/release",
                        "Fantomas.Client.*.nupkg",
                        SearchOption.TopDirectoryOnly
                    )
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
    stage "Fantomas" { run "dotnet fantomas src docs build.fsx" }
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
            if line.Contains("FSharp.Compiler") then
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
            [| "src/Compiler/FSComp.txt"
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
        /// Optional because new releases don't have a published date yet
        PublishedDate: string option
        Draft: string
    }

let mkGithubRelease (v: SemanticVersion, d: DateTime, cd: ChangelogData option) =
    match cd with
    | None -> failwith "Each Fantomas release is expected to have at least one section."
    | Some cd ->
        let version =
            if String.IsNullOrEmpty v.Prerelease then
                $"{v.Major}.{v.Minor}.{v.Patch}"
            else
                $"{v.Major}.{v.Minor}.{v.Patch}-{v.Prerelease}"

        printfn $"Parsing release version: {version} (prerelease: {not (String.IsNullOrEmpty v.Prerelease)})"

        let title =
            let month = d.ToString("MMMM")
            let day = d.Day.Ordinalize()
            $"{month} {day} Release"

        let prefixedVersion = $"v{version}"
        printfn $"Checking if release {prefixedVersion} already exists on GitHub..."

        let publishDate =
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
                Some(dateStr)

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

let getReleaseNotes currentRelease (lastRelease: GithubRelease) =
    let date =
        match lastRelease.PublishedDate with
        | Some d ->
            printfn $"Using last release published date for author attribution: {d}"
            d
        | None ->
            // Query GitHub for the most recent published release
            printfn "Last release has no published date, querying GitHub for most recent release..."
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

let getCurrentAndLastReleaseFromChangelog () =
    printfn "Parsing CHANGELOG.md to find current and last release..."
    let changelog = FileInfo(__SOURCE_DIRECTORY__ </> "CHANGELOG.md")
    let changeLogResult =
        match Parser.parseChangeLog changelog with
        | Error error -> failwithf "Failed to parse changelog: %A" error
        | Ok result ->
            printfn $"Found {result.Releases.Length} releases in changelog"
            result

    let lastReleases =
        changeLogResult.Releases
        |> List.sortByDescending (fun (_, d, _) -> d)
        |> List.take 2

    match lastReleases with
    | [ current; last ] ->
        let currentVersion, _, _ = current
        let lastVersion, _, _ = last
        let currentPrerelease =
            if String.IsNullOrEmpty currentVersion.Prerelease then
                ""
            else
                $" (prerelease: {currentVersion.Prerelease})"
        let lastPrerelease =
            if String.IsNullOrEmpty lastVersion.Prerelease then
                ""
            else
                $" (prerelease: {lastVersion.Prerelease})"
        printfn
            $"Current release: {currentVersion.Major}.{currentVersion.Minor}.{currentVersion.Patch}{currentPrerelease}"
        printfn $"Last release: {lastVersion.Major}.{lastVersion.Minor}.{lastVersion.Patch}{lastPrerelease}"
        mkGithubRelease current, mkGithubRelease last
    | _ -> failwith "Could not find the current and last release from CHANGELOG.md"

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

                let currentRelease, lastRelease = getCurrentAndLastReleaseFromChangelog ()

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
                        Directory.EnumerateFiles("artifacts/package/release", "*.nupkg", SearchOption.TopDirectoryOnly)
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

                    let notes = getReleaseNotes currentRelease lastRelease
                    printfn "Release notes that will be used:"
                    printfn "---"
                    printfn "%s" notes
                    printfn "---"
                    let noteFile = Path.GetTempFileName()
                    File.WriteAllText(noteFile, notes)
                    let files = nugetPackages |> String.concat " "

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
    stage "Clean" { run (cleanFolders [| analysisReportsDir; "artifacts" |]) }
    stage "Build" { run "dotnet build -c Release --tl" }
    stage "Pack" { run "dotnet pack --no-restore -c Release --tl" }
    stage "Publish" {
        run (fun ctx ->
            async {
                let nugetPackages =
                    Directory.EnumerateFiles("artifacts/package/release", "*.nupkg", SearchOption.TopDirectoryOnly)
                    |> Seq.filter (fun nupkg -> not (nupkg.Contains("Fantomas.Client")))
                    |> Seq.toArray

                let! nugetExitCodes = nugetPackages |> Array.map pushPackage |> Async.Sequential

                return Seq.sum nugetExitCodes
            })
    }
    runIfOnlySpecified true
}

pipeline "Analyze" {
    workingDir __SOURCE_DIRECTORY__
    stage "RestoreTools" { run "dotnet tool restore" }
    stage "RestoreSolution" { run "dotnet restore --tl" }
    stage "Analyze" { run "dotnet msbuild /t:AnalyzeSolution" }
    runIfOnlySpecified true
}

tryPrintPipelineCommandHelp ()
