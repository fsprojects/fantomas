#r "nuget: Fun.Build, 0.3.8"
#r "nuget: CliWrap, 3.5.0"
#r "nuget: FSharp.Data, 5.0.2"
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

let cleanFolders (input: string seq) =
    async {
        input
        |> Seq.iter (fun dir ->
            if Directory.Exists(dir) then
                Directory.Delete(dir, true))
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

let getNuGetPackages () =
    Directory.EnumerateFiles("bin", "*.nupkg", SearchOption.TopDirectoryOnly)
    |> Seq.filter (fun nupkg -> not (nupkg.Contains("Fantomas.Client")))

pipeline "Build" {
    workingDir __SOURCE_DIRECTORY__
    stage "RestoreTools" { run "dotnet tool restore" }
    stage "Clean" {
        run (
            cleanFolders
                [| "bin"
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
        run "dotnet fsi ./docs/.style/style.fsx"
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
                                (line.StartsWith("AM") || line.StartsWith("M"))
                                && (line.EndsWith(".fs") || line.EndsWith(".fsx") || line.EndsWith(".fsi"))
                            then
                                Some(line.Replace("AM ", "").Replace("M ", ""))
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
        run "dotnet tool restore"
        run "dotnet build -c Release src/Fantomas/Fantomas.fsproj"
    }
    stage "Watch" {
        paralle
        run "dotnet fsi ./docs/.style/style.fsx --watch"
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
    }

pipeline "Init" {
    workingDir __SOURCE_DIRECTORY__
    stage "Download FCS files" {
        run (fun _ ->
            [| "src/Compiler/FSComp.txt"
               "src/Compiler/Interactive/FSIstrings.txt"
               "src/Compiler/FSStrings.resx"
               "src/Compiler/Utilities/Activity.fsi"
               "src/Compiler/Utilities/Activity.fs"
               "src/Compiler/Utilities/sformat.fsi"
               "src/Compiler/Utilities/sformat.fs"
               "src/Compiler/Utilities/sr.fsi"
               "src/Compiler/Utilities/sr.fs"
               "src/Compiler/Utilities/ResizeArray.fsi"
               "src/Compiler/Utilities/ResizeArray.fs"
               "src/Compiler/Utilities/HashMultiMap.fsi"
               "src/Compiler/Utilities/HashMultiMap.fs"
               "src/Compiler/Utilities/TaggedCollections.fsi"
               "src/Compiler/Utilities/TaggedCollections.fs"
               "src/Compiler/Utilities/illib.fsi"
               "src/Compiler/Utilities/illib.fs"
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
               "src/Compiler/Utilities/QueueList.fsi"
               "src/Compiler/Utilities/QueueList.fs"
               "src/Compiler/Utilities/lib.fsi"
               "src/Compiler/Utilities/lib.fs"
               "src/Compiler/Utilities/rational.fsi"
               "src/Compiler/Utilities/rational.fs"
               "src/Compiler/Utilities/PathMap.fsi"
               "src/Compiler/Utilities/PathMap.fs"
               "src/Compiler/Utilities/RidHelpers.fs"
               "src/Compiler/Utilities/range.fsi"
               "src/Compiler/Utilities/range.fs"
               "src/Compiler/Facilities/UtilsStrings.txt"
               "src/Compiler/Facilities/LanguageFeatures.fsi"
               "src/Compiler/Facilities/LanguageFeatures.fs"
               "src/Compiler/Facilities/DiagnosticOptions.fsi"
               "src/Compiler/Facilities/DiagnosticOptions.fs"
               "src/Compiler/Facilities/TextLayoutRender.fsi"
               "src/Compiler/Facilities/TextLayoutRender.fs"
               "src/Compiler/Facilities/DiagnosticsLogger.fsi"
               "src/Compiler/Facilities/DiagnosticsLogger.fs"
               "src/Compiler/Facilities/prim-lexing.fsi"
               "src/Compiler/Facilities/prim-lexing.fs"
               "src/Compiler/Facilities/prim-parsing.fsi"
               "src/Compiler/Facilities/prim-parsing.fs"
               "src/Compiler/AbstractIL/illex.fsl"
               "src/Compiler/AbstractIL/ilpars.fsy"
               "src/Compiler/AbstractIL/il.fsi"
               "src/Compiler/AbstractIL/il.fs"
               "src/Compiler/AbstractIL/ilx.fsi"
               "src/Compiler/AbstractIL/ilx.fs"
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
    { Version: string
      Title: string
      Date: DateTime
      Exists: bool
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
        let exists =
            let cmdResult =
                Cli
                    .Wrap("gh")
                    .WithArguments($"release view {prefixedVersion}")
                    .WithValidation(CommandResultValidation.None)
                    .ExecuteAsync()
                    .Task.Result
            cmdResult.ExitCode = 0

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
          Exists = exists
          Draft = draft }

let getReleaseNotes currentRelease lastRelease =
    let date = lastRelease.Date.ToString("yyyy-MM-dd")
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
            $"Special thanks to %s{authors.[0]}!"
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

pipeline "Release" {
    workingDir __SOURCE_DIRECTORY__
    // stage "Push to NuGet" {
    //     run (fun _ ->
    //         async {
    //             let! exitCodes =
    //                 getNuGetPackages ()
    //                 |> Seq.map pushPackage
    //                 |> Async.Sequential
    //             return Seq.max exitCodes
    //         })
    // }
    stage "GitHub Release" {
        run (fun _ ->
            async {
                // Get the current and last release from the Changelog
                let changelog = FileInfo(__SOURCE_DIRECTORY__ </> "CHANGELOG.md")
                let changeLogResult =
                    match Parser.parseChangeLog changelog with
                    | Error error -> failwithf "%A" error
                    | Ok result -> result

                let currentRelease, lastRelease =
                    let lastReleases =
                        changeLogResult.Releases
                        |> List.filter (fun (v, _, _) -> String.IsNullOrEmpty v.Prerelease)
                        |> List.sortByDescending (fun (_, d, _) -> d)
                        |> List.take 2

                    match lastReleases with
                    | [ current; last ] -> mkGithubRelease current, mkGithubRelease last
                    | _ -> failwith "Could not find the current and last release from CHANGELOG.md"

                // We create a draft release that requires a manual publish.
                // This is to allow us to add additional release notes when it makes sense.
                if not currentRelease.Exists then
                    let notes = getReleaseNotes currentRelease lastRelease
                    let files = getNuGetPackages () |> String.concat " "
                    printfn "%s\nfiles:%s" notes files
            // Cli
            //     .Wrap("gh")
            //     .WithArguments(
            //         $"release create v{currentRelease.Version} {files} --draft --title {currentRelease.Title} -n \"%s{notes}\""
            //     )
            //     .ExecuteAsync()
            //     .Task.Wait()
            })
    }
    runIfOnlySpecified true
}

// TODO: add GITHUB_TOKEN as env to GitHub Action
