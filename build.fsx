#r "nuget: Fun.Build, 1.0.3"
#r "nuget: CliWrap, 3.6.4"
#r "nuget: Ionide.KeepAChangelog, 0.1.8"
#r "nuget: Humanizer.Core, 2.14.1"

open System
open System.Text.Json
open System.IO
open Fun.Build
open CliWrap
open CliWrap.Buffered
open Ionide.KeepAChangelog
open Ionide.KeepAChangelog.Domain
open SemVersion
open Humanizer

let (</>) a b = Path.Combine(a, b)

let cleanFolders (input: string seq) =
    async {
        input
        |> Seq.iter (fun dir ->
            if Directory.Exists(dir) then
                Directory.Delete(dir, true))
    }

/// Workaround for https://github.com/dotnet/sdk/issues/35989
let restoreTools (ctx: Internal.StageContext) =
    async {
        let json = File.ReadAllText ".config/dotnet-tools.json"
        let jsonDocument = JsonDocument.Parse(json)
        let root = jsonDocument.RootElement
        let tools = root.GetProperty("tools")

        let! installs =
            tools.EnumerateObject()
            |> Seq.map (fun tool ->
                let version = tool.Value.GetProperty("version").GetString()
                ctx.RunCommand $"dotnet tool install %s{tool.Name} --version %s{version}")
            |> Async.Sequential

        let failedInstalls =
            installs
            |> Array.tryPick (function
                | Ok _ -> None
                | Error error -> Some error)

        match failedInstalls with
        | None -> return 0
        | Some error ->
            printfn $"%s{error}"
            return 1
    }

let benchmarkAssembly =
    "src"
    </> "Fantomas.Benchmarks"
    </> "bin"
    </> "Release"
    </> "net7.0"
    </> "Fantomas.Benchmarks.dll"

let semanticVersioning =
    __SOURCE_DIRECTORY__
    </> "src"
    </> "Fantomas"
    </> "bin"
    </> "Release"
    </> "net6.0"
    </> "SemanticVersioning.dll"

let pushPackage nupkg =
    async {
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
    stage "RestoreTools" { run restoreTools }
    stage "Clean" {
        run (
            cleanFolders
                [| analysisReportsDir
                   "bin"
                   "src/Fantomas.FCS/bin/Release"
                   "src/Fantomas.FCS/obj/Release"
                   "src/Fantomas.Core/bin/Release"
                   "src/Fantomas.Core/obj/Release"
                   "src/Fantomas/bin/Release"
                   "src/Fantomas/obj/Release"
                   "src/Fantomas.Client/bin/Release"
                   "src/Fantomas.Client/obj/Release" |]
        )
    }
    stage "CheckFormat" { run "dotnet fantomas src docs build.fsx --check" }
    stage "Build" { run "dotnet build -c Release" }
    stage "UnitTests" { run "dotnet test -c Release" }
    stage "Pack" { run "dotnet pack --no-restore -c Release -o ./bin" }
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
    stage "Prepare" { run "dotnet build -c Release src/Fantomas.Benchmarks" }
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
    stage "Push" {
        run (fun _ ->
            async {
                return!
                    Directory.EnumerateFiles("bin", "Fantomas.Client.*.nupkg", SearchOption.TopDirectoryOnly)
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
        run restoreTools
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

type GithubRelease =
    { Version: string
      Title: string
      Date: DateTime
      PublishedDate: string option
      Draft: string }

let mkGithubRelease (v: SemanticVersion, d: DateTime, cd: ChangelogData option) =
    match cd with
    | None -> failwith "Each Fantomas release is expected to have at least one section."
    | Some cd ->
        let version = $"{v.Major}.{v.Minor}.{v.Patch}"
        let title =
            let month = d.ToString("MMMM")
            let day = d.Day.Ordinalize()
            $"{month} {day} Release"

        let prefixedVersion = $"v{version}"
        let publishDate =
            let cmdResult =
                Cli
                    .Wrap("gh")
                    .WithArguments($"release view {prefixedVersion} --json publishedAt -t \"{{{{.publishedAt}}}}\"")
                    .WithValidation(CommandResultValidation.None)
                    .ExecuteBufferedAsync()
                    .Task.Result
            if cmdResult.ExitCode <> 0 then
                None
            else
                let output = cmdResult.StandardOutput.Trim()
                let lastIdx = output.LastIndexOf("Z", StringComparison.Ordinal)
                Some(output.Substring(0, lastIdx))

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
    let date = lastRelease.PublishedDate.Value
    let authorMsg =
        let authors =
            Cli
                .Wrap("gh")
                .WithArguments(
                    $"pr list -S \"state:closed base:main closed:>{date} -author:app/robot\" --json author --jq \".[].author.login\""
                )
                .ExecuteBufferedAsync()
                .Task.Result.StandardOutput.Split([| '\n' |], StringSplitOptions.RemoveEmptyEntries)
            |> Array.distinct
            |> Array.sort

        if authors.Length = 1 then
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
    let changelog = FileInfo(__SOURCE_DIRECTORY__ </> "CHANGELOG.md")
    let changeLogResult =
        match Parser.parseChangeLog changelog with
        | Error error -> failwithf "%A" error
        | Ok result -> result

    let lastReleases =
        changeLogResult.Releases
        |> List.filter (fun (v, _, _) -> String.IsNullOrEmpty v.Prerelease)
        |> List.sortByDescending (fun (_, d, _) -> d)
        |> List.take 2

    match lastReleases with
    | [ current; last ] -> mkGithubRelease current, mkGithubRelease last
    | _ -> failwith "Could not find the current and last release from CHANGELOG.md"

pipeline "Release" {
    workingDir __SOURCE_DIRECTORY__
    stage "Release" {
        run (fun _ ->
            async {
                let currentRelease, lastRelease = getCurrentAndLastReleaseFromChangelog ()

                if Option.isSome currentRelease.PublishedDate then
                    return 0
                else
                    // Push packages to NuGet
                    let nugetPackages =
                        Directory.EnumerateFiles("bin", "*.nupkg", SearchOption.TopDirectoryOnly)
                        |> Seq.filter (fun nupkg -> not (nupkg.Contains("Fantomas.Client")))
                        |> Seq.toArray

                    let! nugetExitCodes = nugetPackages |> Array.map pushPackage |> Async.Sequential

                    let notes = getReleaseNotes currentRelease lastRelease
                    let noteFile = Path.GetTempFileName()
                    File.WriteAllText(noteFile, notes)
                    let files = nugetPackages |> String.concat " "

                    // We create a draft release for minor and majors. Those that requires a manual publish.
                    // This is to allow us to add additional release notes when it makes sense.
                    let! draftResult =
                        let isDraft =
                            let isRevision = lastRelease.Version.Split('.') |> Array.last |> int |> (<>) 0
                            if isRevision then String.Empty else "--draft"

                        Cli
                            .Wrap("gh")
                            .WithArguments(
                                $"release create v{currentRelease.Version} {files} {isDraft} --title \"{currentRelease.Title}\" --notes-file \"{noteFile}\""
                            )
                            .WithValidation(CommandResultValidation.None)
                            .ExecuteAsync()
                            .Task
                        |> Async.AwaitTask

                    if File.Exists noteFile then
                        File.Delete(noteFile)

                    return Seq.max [| yield! nugetExitCodes; yield draftResult.ExitCode |]
            })
    }
    runIfOnlySpecified true
}

tryPrintPipelineCommandHelp ()
