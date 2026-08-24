#!/usr/bin/env -S dotnet fsi

#r "nuget: Fun.Build, 1.1.16"
#r "nuget: CliWrap, 3.6.4"
#r "nuget: FSharp.Data, 6.3.0"
#r "nuget: Ionide.KeepAChangelog, 0.1.8"
#r "nuget: Humanizer.Core, 2.14.1"

// The build is split across these, and they are loaded in dependency order: each expects the ones
// above it to be in scope and does not load them itself. Loading a file twice would compile it
// twice, and two copies of a type are two different types, so the order lives here and nowhere else.
#load "scripts/BuildCommon.fsx"
#load "scripts/BuildAnalyzers.fsx"
#load "scripts/BuildRelease.fsx"
#load "scripts/BuildCompiler.fsx"

open System
open System.IO
open Fun.Build
open CliWrap
open BuildCommon
open BuildAnalyzers
open BuildRelease
open BuildCompiler

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

pipeline "Build" {
    workingDir __SOURCE_DIRECTORY__
    stage "RestoreTools" { run "dotnet tool restore" }
    stage "Clean" { run (cleanFolders [| analysisReportsDir; artifactsDir |]) }
    stage "CheckFormat" { run "dotnet fantomas src analyzers docs scripts build.fsx --check --json" }
    stage "Build" { run "dotnet build -c Release --tl" }
    stage "UnitTests" { run "dotnet test -c Release --tl" }
    stage "Pack" { run "dotnet pack --no-restore -c Release --tl" }
    stage "Docs" {
        whenNot { platformOSX }
        envVars
            [|
                "DOTNET_ROLL_FORWARD_TO_PRERELEASE", "1"
                "DOTNET_ROLL_FORWARD", "LatestMajor"
            |]
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

                    // CliWrap discards the child's output unless it is given somewhere to put
                    // it, and what fantomas has to say about the files is the point of the run.
                    let toConsole: PipeTarget =
                        PipeTarget.ToDelegate(fun (line: string) -> printfn "%s" line)

                    let! result =
                        Cli
                            .Wrap("dotnet")
                            .WithArguments($"fantomas --json {arguments}")
                            .WithStandardOutputPipe(toConsole)
                            .WithStandardErrorPipe(toConsole)
                            .WithValidation(CommandResultValidation.None)
                            .ExecuteAsync()
                            .Task
                        |> Async.AwaitTask

                    return result.ExitCode
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
            [|
                "DOTNET_ROLL_FORWARD_TO_PRERELEASE", "1"
                "DOTNET_ROLL_FORWARD", "LatestMajor"
            |]
        run
            $"dotnet fsdocs watch --properties Configuration=Release --fscoptions \" -r:{semanticVersioning}\" --eval --nonpublic"
    }
    runIfOnlySpecified true
}

pipeline "FormatAll" {
    workingDir __SOURCE_DIRECTORY__
    stage "Fantomas" { run "dotnet fantomas --json src analyzers docs scripts build.fsx" }
    runIfOnlySpecified true
}

pipeline "EnsureRepoConfig" {
    workingDir __SOURCE_DIRECTORY__
    stage "Git" {
        run "git config core.hooksPath .githooks"
        // Without this, `.git-blame-ignore-revs` is a file git only reads when asked to on the
        // command line. GitHub's blame view honours it on its own; a clone does not.
        run "git config blame.ignoreRevsFile .git-blame-ignore-revs"
        // Mark a line whose real author had to be guessed past an ignored commit, so a skipped
        // attribution is not read as a genuine one.
        run "git config blame.markIgnoredLines true"
    }
    runIfOnlySpecified true
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
                "src/Compiler/SyntaxTree/LexFilter.fs"
            |]
            |> Array.map (downloadCompilerFile fsharpCompilerHash)
            |> Async.Parallel
            |> Async.Ignore)
    }
    runIfOnlySpecified true
}

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

pipeline "Analyze" {
    workingDir __SOURCE_DIRECTORY__
    stage "RestoreTools" { run "dotnet tool restore" }
    stage "RestoreSolution" { run "dotnet restore --tl" }
    stage "BuildAnalyzers" { run buildLocalAnalyzers }
    stage "Analyze" {
        run (fun _ ->
            projectsToAnalyze
            |> List.map (fun (project: string) -> { Project = project; Files = [] })
            |> analyzeTargets excludeLocalAdvisory everyFinding)
    }
    runIfOnlySpecified true
}

// The same analyzers, over the files the working tree touches.
//
// A project is only loaded when it owns a changed file, and is then analyzed for that file alone,
// which is the difference between minutes and seconds on the test projects. What this cannot see
// is a finding a change causes in a file other than the ones you edited, which is what the full
// `Analyze` pipeline is still for before opening a pull request.
pipeline "AnalyzeChanged" {
    workingDir __SOURCE_DIRECTORY__
    stage "RestoreTools" { run "dotnet tool restore" }
    stage "RestoreSolution" { run "dotnet restore --tl" }
    stage "BuildAnalyzers" { run buildLocalAnalyzers }

    stage "Analyze" {
        run (fun _ ->
            async {
                let! files = changedFiles ()

                // Everything reports and nothing fails. Warning rather than something lower
                // because these are still findings to act on, and the tool prints every severity
                // either way; the only thing being given up here is the non-zero exit.
                let demoteLocalErrors: string list = "--treat-as-warning" :: localErrorRules

                match targetsFor files with
                | [] ->
                    printfn "No changed file belongs to a project that is analyzed."
                    return 0
                | targets ->
                    let! scopes = changedLines ()
                    return! analyzeTargets demoteLocalErrors (keepFinding scopes) targets
            })
    }

    runIfOnlySpecified true
}

tryPrintPipelineCommandHelp ()
