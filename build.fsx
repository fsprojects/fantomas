#r "nuget: Fun.Build, 0.1.6"
#r "nuget: CliWrap, 3.5.0"
#r "nuget: FSharp.Data, 5.0.2"

open System.IO
open Fun.Build
open CliWrap
open CliWrap.Buffered
open FSharp.Data
open System.Xml.Linq
open System.Xml.XPath

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
    </> "net6.0"
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
        let! result =
            Cli.Wrap("dotnet").WithArguments($"paket push {nupkg}").ExecuteAsync().Task
            |> Async.AwaitTask
        return result.ExitCode
    }

pipeline "Build" {
    workingDir __SOURCE_DIRECTORY__
    stage "RestoreTools" { run "dotnet tool restore" }
    stage "Clean" {
        run (
            cleanFolders
                [| "bin"
                   "src/Fantomas.FCS/bin"
                   "src/Fantomas.FCS/obj"
                   "src/Fantomas.Core/bin"
                   "src/Fantomas.Core/obj"
                   "src/Fantomas/bin"
                   "src/Fantomas/obj"
                   "src/Fantomas.Client/bin"
                   "src/Fantomas.Client/obj" |]
        )
    }
    stage "CheckFormat" { run "dotnet fantomas src docs build.fsx --recurse --check" }
    stage "Build" { run "dotnet build -c Release" }
    stage "UnitTests" { run "dotnet test -c Release" }
    stage "Benchmark" { run $"dotnet {benchmarkAssembly}" }
    stage "Pack" { run "dotnet pack --no-restore -c Release -o ./bin" }
    stage "Docs" {
        whenNot { platformOSX }
        run "dotnet fsi ./docs/.style/style.fsx"
        run
            $"dotnet fsdocs build --clean --properties Configuration=Release --fscoptions \" -r:{semanticVersioning}\" --eval"
    }
    stage "Push" {
        whenCmdArg "--push"
        run (fun _ ->
            async {
                let! exitCodes =
                    Directory.EnumerateFiles("bin", "*.nupkg", SearchOption.TopDirectoryOnly)
                    |> Seq.filter (fun nupkg -> not (nupkg.Contains("Fantomas.Client")))
                    |> Seq.map pushPackage
                    |> Async.Sequential
                return Seq.max exitCodes
            })
    }
    runIfOnlySpecified false
}

let runGitCommand (arguments: string) =
    async {
        let! result =
            Cli.Wrap("git").WithArguments(arguments).WithWorkingDirectory(
                __SOURCE_DIRECTORY__
            )
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
                let! exitCode, stdout, stdErr = runGitCommand "status --porcelain"
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
        run $"dotnet fsdocs watch --properties Configuration=Release --fscoptions \" -r:{semanticVersioning}\" --eval"
    }
    runIfOnlySpecified true
}

pipeline "FormatAll" {
    workingDir __SOURCE_DIRECTORY__
    stage "Fantomas" { run "dotnet fantomas src docs build.fsx --recurse" }
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
                printfn "Could not download %s" relativePath
            do! Async.AwaitTask(response.ResponseStream.CopyToAsync(fs))
            fs.Close()
    }

pipeline "Init" {
    workingDir __SOURCE_DIRECTORY__
    stage "RestoreTools" { run "dotnet tool restore" }
    stage "Download FCS files" {
        run (fun _ ->
            [| "src/Compiler/FSComp.txt"
               "src/Compiler/Interactive/FSIstrings.txt"
               "src/Compiler/FSStrings.resx"
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
               "src/Compiler/Facilities/Logger.fsi"
               "src/Compiler/Facilities/Logger.fs"
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
