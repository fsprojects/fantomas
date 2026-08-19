module Fantomas.Client.Tests

open System
open System.IO
open System.Text
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

    /// Every fantomas version the tests below drive. Each one is installed once, before any test
    /// runs, so that a test never races another one into a half-prepared directory. Add a version
    /// here when you add a `TestCase` for it.
    ///
    /// The point of these tests is that Fantomas.Client can still talk to a released fantomas, so
    /// the latest stable release of each supported major is enough. Older releases, and the
    /// prereleases of a major that has since shipped, are not worth holding anyone to.
    let versions: string list = [ "6.3.16"; "7.0.5" ]

    let installAttempts: int = 3

    let dotnetIn (workingDirectory: string) (command: string) : Task<BufferedCommandResult> =
        Cli
            .Wrap("dotnet")
            .WithWorkingDirectory(workingDirectory)
            .WithArguments(command)
            // Report a failing command as a result, so it can be retried instead of thrown.
            .WithValidation(CommandResultValidation.None)
            .ExecuteBufferedAsync()
            .Task

    /// Install one fantomas version in its own directory, and prove it runs.
    let install (version: string) : Task<Result<unit, string>> =
        backgroundTask {
            let subDirectory = folder.CreateSubdirectory(version)

            // This sdk version must match the version used in this repository.
            // It will be the version which the CI/CD pipeline has access to.
            let! dotnetVersionResult = dotnetIn __SOURCE_DIRECTORY__ "--version"
            let dotnetVersion = dotnetVersionResult.StandardOutput.Trim()

            let commands =
                [ $"new globaljson --sdk-version %s{dotnetVersion} --roll-forward latestPatch"
                  "new tool-manifest"
                  $"tool install fantomas -v d --version %s{version} --add-source https://api.nuget.org/v3/index.json"
                  // The tool answering here is what the tests below assume. Asking now keeps a
                  // broken install from surfacing as a puzzling assertion failure inside a test.
                  "fantomas --version" ]

            let mutable failure: string option = None

            for command in commands do
                if Option.isNone failure then
                    let! result = dotnetIn subDirectory.FullName command

                    if result.ExitCode <> 0 then
                        failure <-
                            Some
                                $"`dotnet %s{command}` exited with %i{result.ExitCode}.%s{Environment.NewLine}%s{result.StandardOutput}%s{result.StandardError}"

            match failure with
            | Some error -> return Error error
            | None ->
                let fsharpFile = Path.Combine(subDirectory.FullName, "File.fs")
                File.Create(fsharpFile).Dispose()

                // Create a .editorconfig file to override any parent configuration
                let editorConfigPath = Path.Combine(subDirectory.FullName, ".editorconfig")

                let editorConfigContent =
                    """
root = true

[*.fs]
end_of_line = lf
"""

                File.WriteAllText(editorConfigPath, editorConfigContent)
                return Ok()
        }

    /// Installing from NuGet is the flaky part of these tests, so give it a couple of goes before
    /// declaring the whole fixture unusable.
    let rec installWithRetry (version: string) (attemptsLeft: int) : Task =
        backgroundTask {
            let! result = install version

            match result with
            | Ok() -> ()
            | Error error when attemptsLeft > 1 ->
                TestContext.Progress.WriteLine $"Installing fantomas %s{version} failed, retrying. %s{error}"
                Directory.Delete(Path.Combine(folder.FullName, version), true)
                do! Task.Delay(TimeSpan.FromSeconds 5.)
                do! installWithRetry version (attemptsLeft - 1)
            | Error error ->
                failwith
                    $"Could not install fantomas %s{version} in %i{installAttempts} attempts. Last failure:%s{Environment.NewLine}%s{error}"
        }

    /// What the daemon answered, and enough of its surroundings to explain why. These tests fail
    /// on CI now and then with a response nobody expects, `IgnoredFile` in particular, and the
    /// bare assertion only reports a number. Everything the daemon consults to reach that answer
    /// is listed here: the file it was asked about, the `.fantomasignore` and `.editorconfig`
    /// files it can find by walking up from that file, where the temp directory actually is, and
    /// which fantomas the manifest resolves to.
    let report (fsharpFile: string) (expected: FantomasResponseCode) (response: FantomasResponse) : Task<string> =
        backgroundTask {
            let sb = StringBuilder()
            let line (text: string) = sb.AppendLine(text) |> ignore

            line
                $"Expected %A{expected} (%i{int expected}) but the daemon answered %A{enum<FantomasResponseCode> response.Code} (%i{response.Code})."

            line $"Response file path: %s{response.FilePath}"

            match response.Content with
            | Some content -> line $"Response content: %s{content}"
            | None -> line "Response content: <none>"

            line ""
            line $"Requested file: %s{fsharpFile} (exists: %b{File.Exists fsharpFile})"
            line $"Path.GetTempPath(): %s{Path.GetTempPath()}"

            let tmpdir =
                Environment.GetEnvironmentVariable "TMPDIR"
                |> Option.ofObj
                |> Option.defaultValue "<unset>"

            line $"TMPDIR: %s{tmpdir}"
            line $"Working directory of this test host: %s{Directory.GetCurrentDirectory()}"

            line ""
            line "Configuration files found by walking up from the requested file:"
            let mutable directory = FileInfo(fsharpFile).Directory
            let mutable foundAny = false

            while not (isNull directory) do
                for name in [ ".fantomasignore"; ".editorconfig" ] do
                    let candidate = Path.Combine(directory.FullName, name)

                    if File.Exists candidate then
                        foundAny <- true
                        line $"  %s{candidate}"

                        for content in File.ReadAllLines candidate do
                            line $"    | %s{content}"

                directory <- directory.Parent

            if not foundAny then
                line "  <none>"

            let versionDirectory = Path.GetDirectoryName(fsharpFile: string)

            line ""
            let! toolList = dotnetIn versionDirectory "tool list"
            line $"dotnet tool list (exit %i{toolList.ExitCode}):"
            line (toolList.StandardOutput.TrimEnd())

            let! toolVersion = dotnetIn versionDirectory "fantomas --version"
            line $"dotnet fantomas --version (exit %i{toolVersion.ExitCode}): %s{toolVersion.StandardOutput.Trim()}"

            if not (String.IsNullOrWhiteSpace toolVersion.StandardError) then
                line $"  stderr: %s{toolVersion.StandardError.Trim()}"

            return sb.ToString()
        }

    /// Assert on the response code, and say everything we know when it is not the expected one.
    let expectResponse (expected: FantomasResponseCode) (fsharpFile: string) (response: FantomasResponse) : Task =
        backgroundTask {
            if response.Code <> int expected then
                let! report = report fsharpFile expected response
                Assert.Fail report
        }

    let withVersion version (callback: string -> Task) =
        backgroundTask {
            let file = Path.Combine(folder.FullName, version, "File.fs")

            if not (File.Exists file) then
                failwith $"fantomas %s{version} was never installed. Add it to `versions` in this fixture."

            do! callback file
        }

    [<OneTimeSetUp>]
    member _.Setup() : Task =
        backgroundTask {
            folder.Create()

            for version in versions do
                do! installWithRetry version installAttempts
        }

    [<OneTimeTearDown>]
    member _.TearDown() =
        backgroundTask {
            service.Dispose()
            // Give it a little time before all processes are truly killed.
            do! Task.Delay(200)
            folder.Delete(true)
        }

    [<TestCase("6.3.16")>]
    [<TestCase("7.0.5")>]
    member _.Version(version: string) =
        withVersion version (fun fsharpFile ->
            backgroundTask {
                let! response = service.VersionAsync(fsharpFile)
                do! expectResponse FantomasResponseCode.Version fsharpFile response
            })

    [<TestCase("6.3.16")>]
    [<TestCase("7.0.5")>]
    member _.FormatDocument(version: string) =
        withVersion version (fun fsharpFile ->
            backgroundTask {
                let request: FormatDocumentRequest =
                    { SourceCode = unformattedCode
                      FilePath = fsharpFile
                      Config = None
                      Cursor = None }

                let! response = service.FormatDocumentAsync(request)
                do! expectResponse FantomasResponseCode.Formatted fsharpFile response
            })

    [<TestCase("6.3.16")>]
    [<TestCase("7.0.5")>]
    member _.``FormatDocument with Cursor``(version: string) =
        withVersion version (fun fsharpFile ->
            backgroundTask {
                let request: FormatDocumentRequest =
                    { SourceCode = unformattedCode
                      FilePath = fsharpFile
                      Config = None
                      Cursor = Some(FormatCursorPosition(1, 12)) }

                let! response = service.FormatDocumentAsync(request)
                do! expectResponse FantomasResponseCode.Formatted fsharpFile response
            })
