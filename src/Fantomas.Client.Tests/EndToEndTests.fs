module Fantomas.Client.Tests

open System
open System.IO
open System.Threading.Tasks
open CliWrap
open CliWrap.Buffered
open Fantomas.Client.Contracts
open Fantomas.Client.LSPFantomasService
open Fantomas.Client.LSPFantomasServiceTypes
open NUnit.Framework

[<TestFixture>]
type EndToEndTests() =
    let folder: DirectoryInfo =
        DirectoryInfo(Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString("N")))

    let service: FantomasService = new LSPFantomasService()

    let unformattedCode = "let a    =    8"

    let withVersion version (callback: string -> Task) =
        if Path.Exists(Path.Combine(folder.FullName, version)) then
            backgroundTask {
                let file = Path.Combine(folder.FullName, version, "File.fs")
                do! callback file
            }
        else
            backgroundTask {
                let subDirectory = folder.CreateSubdirectory(version)

                let dotnet (command: string) =
                    Cli
                        .Wrap("dotnet")
                        .WithWorkingDirectory(subDirectory.FullName)
                        .WithArguments(command)
                        .ExecuteBufferedAsync()
                        .Task
                    :> Task

                // This sdk version must match the version used in this repository.
                // It will be the version which the CI/CD pipeline has access to.
                do! dotnet "new globaljson --sdk-version 7.0.202 --roll-forward latestPatch"
                do! dotnet "new tool-manifest"

                do!
                    dotnet
                        $"tool install fantomas -v d --version {version} --add-source https://api.nuget.org/v3/index.json"

                let fsharpFile = Path.Combine(subDirectory.FullName, "File.fs")
                File.Create(fsharpFile).Dispose()
                do! callback fsharpFile
            }

    [<OneTimeSetUp>]
    member _.Setup() = folder.Create()

    [<OneTimeTearDown>]
    member _.TearDown() =
        backgroundTask {
            service.Dispose()
            // Give it a little time before all processes are truly killed.
            do! Task.Delay(200)
            folder.Delete(true)
        }

    [<TestCase("5.0.6")>]
    [<TestCase("5.1.5")>]
    [<TestCase("5.2.2")>]
    [<TestCase("6.0.0-alpha-004")>]
    member _.Version(version: string) =
        withVersion version (fun fsharpFile ->
            backgroundTask {
                let! version = service.VersionAsync(fsharpFile)
                Assert.AreEqual(int FantomasResponseCode.Version, version.Code)
            })

    [<TestCase("5.0.6")>]
    [<TestCase("5.1.5")>]
    [<TestCase("5.2.2")>]
    [<TestCase("6.0.0-alpha-004")>]
    member _.FormatDocument(version: string) =
        withVersion version (fun fsharpFile ->
            backgroundTask {
                let request: FormatDocumentRequest =
                    { SourceCode = unformattedCode
                      FilePath = fsharpFile
                      Config = None
                      Cursor = None }

                let! formatResponse = service.FormatDocumentAsync(request)
                Assert.AreEqual(int FantomasResponseCode.Formatted, formatResponse.Code)
            })

    [<TestCase("6.0.0-alpha-004")>]
    member _.``FormatDocument with Cursor``(version: string) =
        withVersion version (fun fsharpFile ->
            backgroundTask {
                let request: FormatDocumentRequest =
                    { SourceCode = unformattedCode
                      FilePath = fsharpFile
                      Config = None
                      Cursor = Some(FormatCursorPosition(1, 12)) }

                let! formatResponse = service.FormatDocumentAsync(request)
                Assert.AreEqual(int FantomasResponseCode.Formatted, formatResponse.Code)
            })
