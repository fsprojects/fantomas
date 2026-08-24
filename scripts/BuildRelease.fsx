#r "nuget: CliWrap, 3.6.4"
#r "nuget: FSharp.Data, 6.3.0"
#r "nuget: Ionide.KeepAChangelog, 0.1.8"
#r "nuget: Humanizer.Core, 2.14.1"

open System
open System.IO
open CliWrap
open CliWrap.Buffered
open FSharp.Data
open Ionide.KeepAChangelog
open Ionide.KeepAChangelog.Domain
open SemVersion
open Humanizer
// Loaded by `build.fsx`, after `BuildCommon.fsx`. An error here saying BuildCommon is not defined
// means this file was run on its own; it is a library, so run a pipeline from build.fsx instead.
open BuildCommon

// Working out what a release is: which version is being cut, what changed since the last one, and
// the notes that go with it. Reading only, apart from `pushPackage`; the pipelines decide what to do
// with any of it.

/// Whether this run was asked not to publish anything.
let isDryRun: bool =
    let args = fsi.CommandLineArgs
    Array.exists (fun arg -> arg = "--dry-run") args

/// Push a package to NuGet, unless this run was told not to publish.
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
    let changelog = FileInfo(repositoryRoot </> "CHANGELOG.md")

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
