module Fantomas.Tests.Integration.DaemonTests

open System
open System.Threading.Tasks
open Fantomas
open Fantomas.Client.LSPFantomasServiceTypes
open Fantomas.Daemon
open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelpers
open Fantomas.Core
open Fantomas.Client.Contracts
open Nerdbank.Streams
open StreamJsonRpc

// Which arguments are refused is settled in ArgumentsTests. What is left here is that the process
// acts on it: standard out in daemon mode carries the JSON-RPC protocol, so a run that would have
// written anything else to it must not start at all.
[<Test>]
[<TestCase("--json")>]
[<TestCase("--check")>]
[<TestCase("--force")>]
let ``an argument that means nothing to a daemon stops it starting`` (argument: string) : unit =
    let {
            ExitCode = exitCode
            Output = output
            Error = error
        } =
        runFantomasTool [ "--daemon"; argument ]

    exitCode |> should equal 1
    output |> should equal ""
    Assert.That(error, Does.Contain $"A daemon cannot be combined with %s{argument}")

[<Test>]
let ``every argument that means nothing to a daemon is named at once`` () =
    let { Error = error } =
        runFantomasTool [ "--daemon"; "--force"; "--out"; "build"; "A.fs" ]

    Assert.That(error, Does.Contain "A daemon cannot be combined with --force, --out, input paths")

// `--daemon` is the older spelling and has to go on working: `Fantomas.Client` launches every one
// of its three ways with it, so an editor built against an earlier Fantomas is talking to whatever
// version the user has installed.
[<Test>]
[<TestCase("--daemon")>]
[<TestCase("daemon")>]
let ``both spellings of the daemon refuse the same argument`` (spelling: string) : unit =
    let { ExitCode = exitCode; Error = error } = runFantomasTool [ spelling; "--check" ]

    exitCode |> should equal 1
    Assert.That(error, Does.Contain "A daemon cannot be combined with --check")

// The nudge toward the newer spelling is for a person at a terminal. A pipeline cannot act on it
// and would carry it on every run for as long as both spellings exist, which is forever, so a run
// with standard error redirected must not carry it. That is also what keeps it out of the daemon
// an editor starts, since `Fantomas.Client` redirects that stream in order to read it.
[<Test>]
[<TestCase("--check")>]
[<TestCase("--daemon")>]
let ``the older spelling says nothing when standard error is redirected`` (spelling: string) : unit =
    let { Error = error } = runFantomasTool [ spelling; "--out"; "build" ]

    Assert.That(error, Does.Not.Contain "is how it is spelled now")

let private assertFormatted (actual: string) (expected: string) : unit =
    String.normalizeNewLine actual
    |> should equal (String.normalizeNewLine expected)

let private runWithDaemon (fn: JsonRpc -> Async<unit>) : Async<unit> =
    async {
        let struct (serverStream, clientStream) = FullDuplexStream.CreatePair()

        let daemon = new FantomasDaemon(serverStream, serverStream, realDaemonEnvironment)

        let client = new JsonRpc(clientStream, clientStream)
        client.StartListening()
        do! fn client
        client.Dispose()
        (daemon :> IDisposable).Dispose()
    }

/// As `runWithDaemon`, but over the given environment and listening for the configuration warnings
/// the daemon pushes. The handler has to be attached before the client starts listening.
let private runWithDaemonCollectingWarnings
    (environment: DaemonEnvironment)
    (fn: JsonRpc -> Async<unit>)
    : Async<ConfigurationWarning list>
    =
    async {
        let struct (serverStream, clientStream) = FullDuplexStream.CreatePair()

        let daemon = new FantomasDaemon(serverStream, serverStream, environment)

        let client = new JsonRpc(clientStream, clientStream)
        let warnings = ResizeArray<ConfigurationWarning>()

        client.AddLocalRpcMethod(
            Methods.ConfigurationWarning,
            Action<ConfigurationWarning>(fun warning -> lock warnings (fun () -> warnings.Add warning))
        )

        client.StartListening()

        // The notification is one-way, so it can still be in flight when the response arrives.
        // Poll rather than sleep a fixed amount: a machine under load can take longer than any
        // constant we would pick here, and a test that sleeps too little fails intermittently.
        //
        // Waiting for the count to hold still rather than for the first one to land, so that a test
        // asking for exactly one warning can tell that a second one did not follow it.
        let count () =
            lock warnings (fun () -> warnings.Count)

        let rec settle (attemptsLeft: int) (steadyFor: int) : Async<unit> =
            async {
                let before = count ()

                if (before > 0 && steadyFor >= 2) || attemptsLeft = 0 then
                    return ()
                else

                do! Async.Sleep 50
                let steadyFor = if count () = before then steadyFor + 1 else 0
                return! settle (attemptsLeft - 1) steadyFor
            }

        try
            do! fn client
            do! settle 100 0
        finally
            // A failing assertion must not leave the connection and the daemon behind.
            client.Dispose()
            (daemon :> IDisposable).Dispose()

        return List.ofSeq warnings
    }

/// An environment that reads no `.editorconfig` at all, for a test about what the daemon does with
/// a configuration rather than about where the configuration came from.
let private noEditorConfig: DaemonEnvironment =
    daemonEnvironment (new System.IO.Abstractions.FileSystem()) (fun _ -> None)

/// The single warning the daemon sent, or a failure naming what it sent instead.
let private theOnlyWarning (warnings: ConfigurationWarning list) : ConfigurationWarning =
    match warnings with
    | [ warning ] -> warning
    | otherwise ->

    Assert.Fail $"Expected exactly one configuration warning, got %A{otherwise}"
    failwith "unreachable"

let private problemsOf (warning: ConfigurationWarning) : (int * int * string * string) array =
    warning.Problems
    |> Array.map (fun problem -> problem.Code, problem.Source, problem.Setting, problem.Value)

[<Test>]
let ``version request`` () =
    runWithDaemon (fun client ->
        async {
            let! version = client.InvokeAsync<string>(Methods.Version) |> Async.AwaitTask
            version |> should equal (CodeFormatter.GetVersion())
        }
    )

[<Test>]
let ``config request`` () =
    runWithDaemon (fun client ->
        async {
            let! config = client.InvokeAsync<string>(Methods.Configuration) |> Async.AwaitTask

            FormatConfig.Default
            |> Fantomas.EditorConfig.configToEditorConfig
            |> fun s -> s.Split('\n')
            |> Seq.map (fun line -> line.Split('=').[0])
            |> Seq.iter (fun setting ->
                Assert.That(config.Contains(setting), Is.True, $"Setting %s{setting} not found")
            )
        }
    )

[<Test>]
let ``format implementation file`` () =
    runWithDaemon (fun client ->
        async {
            let sourceCode = "module   Foobar"
            use codeFile = new TemporaryFileCodeSample(sourceCode)

            let request =
                {
                    SourceCode = sourceCode
                    FilePath = codeFile.Filename
                    Config = None
                    Cursor = None
                }

            let! response =
                client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                |> Async.AwaitTask

            match response with
            | FormatDocumentResponse.Formatted(formattedContent = formatted) ->
                assertFormatted
                    formatted
                    "module Foobar
"
            | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
        }
    )

[<Test>]
let ``format implementation file, unchanged`` () =
    runWithDaemon (fun client ->
        async {
            let sourceCode = "module Foobar\n"
            use codeFile = new TemporaryFileCodeSample(sourceCode)

            let request =
                {
                    SourceCode = sourceCode
                    FilePath = codeFile.Filename
                    Config = Some(readOnlyDict [ "end_of_line", "lf" ])
                    Cursor = None
                }

            let! response =
                client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                |> Async.AwaitTask

            match response with
            | FormatDocumentResponse.Unchanged _ -> Assert.Pass()
            | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
        }
    )

[<Test>]
let ``format implementation file, error`` () =
    runWithDaemon (fun client ->
        async {
            let sourceCode = "let foo ="
            use codeFile = new TemporaryFileCodeSample(sourceCode)

            let request =
                {
                    SourceCode = sourceCode
                    FilePath = codeFile.Filename
                    Config = None
                    Cursor = None
                }

            let! response =
                client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                |> Async.AwaitTask

            match response with
            | FormatDocumentResponse.Error _ -> Assert.Pass()
            | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
        }
    )

[<Test>]
let ``format implementation file, ignored file`` () =
    runWithDaemon (fun client ->
        async {
            let sourceCode = "let foo = 4"
            use codeFile = new TemporaryFileCodeSample(sourceCode)
            use _ignoreFixture = new FantomasIgnoreFile("*.fs")

            let request =
                {
                    SourceCode = sourceCode
                    FilePath = codeFile.Filename
                    Config = None
                    Cursor = None
                }

            let! response =
                client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                |> Async.AwaitTask

            match response with
            | FormatDocumentResponse.IgnoredFile _ -> Assert.Pass()
            | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
        }
    )

[<Test>]
let ``format signature file`` () =
    runWithDaemon (fun client ->
        async {
            let sourceCode = "module Foobar\n\nval meh :  int"

            use codeFile = new TemporaryFileCodeSample(sourceCode, extension = "fsi")

            let request =
                {
                    SourceCode = sourceCode
                    FilePath = codeFile.Filename
                    Config = None
                    Cursor = None
                }

            let! response =
                client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                |> Async.AwaitTask

            match response with
            | FormatDocumentResponse.Formatted(formattedContent = formatted) ->
                assertFormatted
                    formatted
                    "module Foobar

val meh: int
"
            | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
        }
    )

[<Test>]
let ``format document respecting .editorconfig file`` () =
    runWithDaemon (fun client ->
        async {
            let sourceCode = "module Foo\n\nlet a = //\n    4"
            use codeFile = new ConfiguredCodeSample("[*.fs]\nindent_size=2", sourceCode)

            let request =
                {
                    SourceCode = sourceCode
                    FilePath = codeFile.Filename
                    Config = None
                    Cursor = None
                }

            let! response =
                client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                |> Async.AwaitTask

            match response with
            | FormatDocumentResponse.Formatted(formattedContent = formatted) ->
                assertFormatted
                    formatted
                    "module Foo

let a = //
  4
"
            | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
        }
    )

[<Test>]
let ``custom configuration has precedence over .editorconfig file`` () =
    runWithDaemon (fun client ->
        async {
            let sourceCode = "module Foo\n\nlet a = //\n    4"
            use codeFile = new ConfiguredCodeSample("[*.fs]\nindent_size=2", sourceCode)

            let request =
                {
                    SourceCode = sourceCode
                    FilePath = codeFile.Filename
                    Config = Some(readOnlyDict [ "indent_size", "4" ])
                    Cursor = None
                }

            let! response =
                client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                |> Async.AwaitTask

            match response with
            | FormatDocumentResponse.Formatted(formattedContent = formatted) ->
                assertFormatted
                    formatted
                    "module Foo

let a = //
    4
"
            | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
        }
    )

[<Test>]
let ``format selection`` () =
    runWithDaemon (fun client ->
        async {
            let sourceCode =
                """module Foo

let    x     = 4
let    y     = 5
            """

            use codeFile = new TemporaryFileCodeSample(sourceCode)

            let request: FormatSelectionRequest =
                let range = FormatSelectionRange(3, 0, 3, 16)

                {
                    SourceCode = sourceCode
                    FilePath = codeFile.Filename
                    Config = None
                    Range = range
                }

            let! response =
                client.InvokeAsync<FormatSelectionResponse>(Methods.FormatSelection, request)
                |> Async.AwaitTask

            match response with
            | FormatSelectionResponse.Formatted(fileName, formatted, _) ->
                fileName |> should equal codeFile.Filename
                assertFormatted formatted "let x = 4"
            | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
        }
    )

[<Test>]
let ``format selection, fsi`` () =
    runWithDaemon (fun client ->
        async {
            let sourceCode =
                """module Foo

val    x     : int
val    y     : string
            """

            use codeFile = new TemporaryFileCodeSample(sourceCode, extension = "fsi")

            let request: FormatSelectionRequest =
                let range = FormatSelectionRange(3, 0, 3, 18)

                {
                    SourceCode = sourceCode
                    FilePath = codeFile.Filename
                    Config = None
                    Range = range
                }

            let! response =
                client.InvokeAsync<FormatSelectionResponse>(Methods.FormatSelection, request)
                |> Async.AwaitTask

            match response with
            | FormatSelectionResponse.Formatted(fileName, formatted, _) ->
                fileName |> should equal codeFile.Filename
                assertFormatted formatted "val x: int"
            | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
        }
    )

[<Test>]
let ``format document with both .editorconfig file and custom config`` () =
    runWithDaemon (fun client ->
        async {
            let sourceCode = "module Foo\n\nlet add (a:int) (b:int) = //\n    a + b"

            use codeFile = new ConfiguredCodeSample("[*.fs]\nindent_size=2", sourceCode)

            let request =
                {
                    SourceCode = sourceCode
                    FilePath = codeFile.Filename
                    Config = Some(readOnlyDict [ "fsharp_space_before_colon", "true" ])
                    Cursor = None
                }

            let! response =
                client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                |> Async.AwaitTask

            match response with
            | FormatDocumentResponse.Formatted(formattedContent = formatted) ->
                assertFormatted
                    formatted
                    "module Foo

let add (a : int) (b : int) = //
  a + b
"
            | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
        }
    )

let private sourceCode = "module Foo\n\nlet add a  b = a + b"

let private documentRequest (filePath: string) (config: (string * string) list option) : FormatDocumentRequest =
    {
        SourceCode = sourceCode
        FilePath = filePath
        Config = config |> Option.map readOnlyDict
        Cursor = None
    }

/// Format one document over the given environment and collect what the daemon reported about it.
let private warningsForDocument
    (environment: DaemonEnvironment)
    (request: FormatDocumentRequest)
    : ConfigurationWarning list
    =
    runWithDaemonCollectingWarnings
        environment
        (fun client ->
            async {
                let! _response =
                    client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                    |> Async.AwaitTask

                return ()
            }
        )
    |> Async.RunSynchronously

[<Test>]
let ``a configuration warning is sent for settings the daemon cannot use`` () =
    use codeFile = new TemporaryFileCodeSample(sourceCode)

    let warning =
        documentRequest
            codeFile.Filename
            (Some [ "fsharp_bogus_option", "true"; "fsharp_experimental_elmish", "not_a_bool" ])
        |> warningsForDocument noEditorConfig
        |> theOnlyWarning

    warning.FilePath |> should equal codeFile.Filename

    problemsOf warning
    |> should
        equal
        [|
            int ConfigurationProblemCode.UnknownSetting,
            int ConfigurationProblemSource.Request,
            "fsharp_bogus_option",
            null
            int ConfigurationProblemCode.UnrecognizedValue,
            int ConfigurationProblemSource.Request,
            "fsharp_experimental_elmish",
            "not_a_bool"
        |]

[<Test>]
let ``a configuration warning with no problems is sent when the configuration is fine`` () =
    use codeFile = new TemporaryFileCodeSample(sourceCode)

    let warning =
        documentRequest codeFile.Filename (Some [ "fsharp_space_before_colon", "true" ])
        |> warningsForDocument noEditorConfig
        |> theOnlyWarning

    // Sent even when there is nothing wrong, so a client can clear what it reported earlier, and
    // without the file list, which says nothing while nothing is wrong.
    warning.Problems |> should be Empty
    warning.EditorConfigFiles |> should be Empty

// The settings an editor sends are checked the same way the ones on disk are. A mistake in them
// must not disturb the response: in daemon mode standard out carries the JSON-RPC protocol, so a
// warning that reached it would fault the connection rather than merely be noisy.
[<Test>]
let ``format document with a custom config the daemon cannot use`` () =
    runWithDaemon (fun client ->
        async {
            use codeFile = new TemporaryFileCodeSample(sourceCode)

            let request =
                documentRequest
                    codeFile.Filename
                    (Some [ "fsharp_bogus_option", "true"; "fsharp_experimental_elmish", "not_a_bool" ])

            let! response =
                client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                |> Async.AwaitTask

            match response with
            | FormatDocumentResponse.Formatted(formattedContent = formatted) ->
                assertFormatted formatted "module Foo\n\nlet add a b = a + b\n"
            | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
        }
    )

[<Test>]
let ``settings read from disk are reported as coming from the editorconfig, with the files`` () =
    use codeFile = new TemporaryFileCodeSample(sourceCode)

    // The reading itself is `EditorConfigurationTests`' business. What matters here is that the
    // daemon passes on what it was given, tagged as having come from a file rather than a request.
    let readConfiguration _ : EditorConfig.EditorConfigResult option =
        Some
            {
                Config = FormatConfig.Default
                EditorConfigFiles = [ "/repo/.editorconfig"; "/repo/src/.editorconfig" ]
                Problems =
                    [
                        EditorConfig.EditorConfigProblem.UnknownSetting "fsharp_bogus_option"
                        EditorConfig.EditorConfigProblem.UnrecognizedValue("fsharp_experimental_elmish", "not_a_bool")
                    ]
            }

    let environment =
        daemonEnvironment (new System.IO.Abstractions.FileSystem()) readConfiguration

    let warning =
        documentRequest codeFile.Filename None
        |> warningsForDocument environment
        |> theOnlyWarning

    problemsOf warning
    |> should
        equal
        [|
            int ConfigurationProblemCode.UnknownSetting,
            int ConfigurationProblemSource.EditorConfig,
            "fsharp_bogus_option",
            null
            int ConfigurationProblemCode.UnrecognizedValue,
            int ConfigurationProblemSource.EditorConfig,
            "fsharp_experimental_elmish",
            "not_a_bool"
        |]

    // The files that contributed, so a client can point the user at them.
    warning.EditorConfigFiles
    |> should equal [| "/repo/.editorconfig"; "/repo/src/.editorconfig" |]

[<Test>]
let ``the editorconfig on disk and the request are reported side by side`` () =
    use codeFile = new TemporaryFileCodeSample(sourceCode)

    let readConfiguration _ : EditorConfig.EditorConfigResult option =
        Some
            {
                Config = FormatConfig.Default
                EditorConfigFiles = [ "/repo/.editorconfig" ]
                Problems = [ EditorConfig.EditorConfigProblem.UnknownSetting "fsharp_from_the_file" ]
            }

    let environment =
        daemonEnvironment (new System.IO.Abstractions.FileSystem()) readConfiguration

    let warning =
        documentRequest codeFile.Filename (Some [ "fsharp_from_the_request", "true" ])
        |> warningsForDocument environment
        |> theOnlyWarning

    warning.Problems
    |> Array.map (fun problem -> problem.Setting, problem.Source)
    |> should
        equal
        [|
            "fsharp_from_the_file", int ConfigurationProblemSource.EditorConfig
            "fsharp_from_the_request", int ConfigurationProblemSource.Request
        |]

[<Test>]
let ``a request setting is reported the way the editor spelled it`` () =
    use codeFile = new TemporaryFileCodeSample(sourceCode)

    let warning =
        documentRequest codeFile.Filename (Some [ "FSHARP_Bogus_Option", "true" ])
        |> warningsForDocument noEditorConfig
        |> theOnlyWarning

    warning.Problems
    |> Array.map (fun problem -> problem.Setting)
    |> should equal [| "FSHARP_Bogus_Option" |]

// Reading the configuration can raise rather than come back with a problem. The client hears about
// that as an Error response, and still gets a warning, so it does not keep showing what the
// previous request left it with.
[<Test>]
let ``a configuration that cannot be read at all is an error, and clears the warning`` () =
    use codeFile = new TemporaryFileCodeSample(sourceCode)

    let warnings =
        runWithDaemonCollectingWarnings
            noEditorConfig
            (fun client ->
                async {
                    let request = documentRequest codeFile.Filename (Some [ "end_of_line", "cr" ])

                    let! response =
                        client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                        |> Async.AwaitTask

                    match response with
                    | FormatDocumentResponse.Error(_, message) ->
                        Assert.That(message, Does.Contain "Carriage returns are not valid")
                    | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
                }
            )
        |> Async.RunSynchronously

    (theOnlyWarning warnings).Problems |> should be Empty

/// A `ReadConfiguration` that holds each caller inside it long enough to overlap with another, and
/// remembers how many were ever inside at once.
let private countingOverlaps () =
    let inFlight = ref 0
    let peak = ref 0

    let readConfiguration (_: string) : EditorConfig.EditorConfigResult option =
        let now = System.Threading.Interlocked.Increment inFlight

        let rec recordPeak () =
            let seen = peak.Value

            if now > seen then
                if System.Threading.Interlocked.CompareExchange(peak, now, seen) <> seen then
                    recordPeak ()

        recordPeak ()
        System.Threading.Thread.Sleep 100
        System.Threading.Interlocked.Decrement inFlight |> ignore
        None

    readConfiguration, (fun () -> peak.Value)

/// Send two format requests without waiting for the first, and report how many of them were ever
/// inside `ReadConfiguration` at the same time.
let private overlapOf (firstFile: string) (secondFile: string) : int =
    let readConfiguration, peak = countingOverlaps ()

    let environment =
        daemonEnvironment (new System.IO.Abstractions.FileSystem()) readConfiguration

    let struct (serverStream, clientStream) = FullDuplexStream.CreatePair()
    let daemon = new FantomasDaemon(serverStream, serverStream, environment)
    let client = new JsonRpc(clientStream, clientStream)
    client.StartListening()

    try
        let send (file: string) =
            client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, documentRequest file None)

        // Started together, awaited afterwards, so both are in flight at once.
        let first = send firstFile
        let second = send secondFile
        Task.WaitAll(first :> Task, second :> Task)
        peak ()
    finally
        client.Dispose()
        (daemon :> IDisposable).Dispose()

// A configuration warning carries a file path and nothing that says which request it belongs to, so
// two requests in flight for one file could deliver theirs in either order and a client had no way
// to tell. The daemon serves one request at a time per file instead.
[<Test>]
let ``two requests for the same file do not overlap`` () =
    use codeFile = new TemporaryFileCodeSample(sourceCode)
    overlapOf codeFile.Filename codeFile.Filename |> should equal 1

// One file reached through two spellings is still one file on Windows and on a default macOS
// volume, and nothing canonicalises the path on the way in, so the gates fold case.
[<Test>]
let ``two spellings of one file share a gate`` () =
    use codeFile = new TemporaryFileCodeSample(sourceCode)

    let shouted =
        System.IO.Path.Combine(
            System.IO.Path.GetDirectoryName codeFile.Filename,
            (System.IO.Path.GetFileName codeFile.Filename).ToUpperInvariant()
        )

    overlapOf codeFile.Filename shouted |> should equal 1

// Only the same file waits. Formatting a repository is the whole point of having several in flight.
[<Test>]
let ``two requests for different files still run at the same time`` () =
    use firstFile = new TemporaryFileCodeSample(sourceCode)
    use secondFile = new TemporaryFileCodeSample(sourceCode)
    overlapOf firstFile.Filename secondFile.Filename |> should equal 2

[<Test>]
let ``format selection reports configuration warnings too`` () =
    use codeFile = new TemporaryFileCodeSample(sourceCode)

    let warnings =
        runWithDaemonCollectingWarnings
            noEditorConfig
            (fun client ->
                async {
                    let request =
                        {
                            SourceCode = sourceCode
                            FilePath = codeFile.Filename
                            Config = Some(readOnlyDict [ "fsharp_bogus_option", "true" ])
                            Range = FormatSelectionRange(3, 0, 3, 19)
                        }

                    let! _response =
                        client.InvokeAsync<FormatSelectionResponse>(Methods.FormatSelection, request)
                        |> Async.AwaitTask

                    return ()
                }
            )
        |> Async.RunSynchronously

    (theOnlyWarning warnings).Problems
    |> Array.map (fun problem -> problem.Code, problem.Setting)
    |> should equal [| int ConfigurationProblemCode.UnknownSetting, "fsharp_bogus_option" |]

[<Test>]
let ``an ignored file still reports an empty configuration warning`` () =
    let fs = System.IO.Abstractions.TestingHelpers.MockFileSystem()
    let root = mockRoot fs
    let ignoredFile = fs.Path.Combine(root, "warned", "Ignored.fs")

    makeFileHierarchy fs [ ignoredFile ]
    fs.File.WriteAllText(fs.Path.Combine(root, IgnoreFile.IgnoreFileName), "warned/Ignored.fs")

    let warning =
        documentRequest ignoredFile None
        |> warningsForDocument (daemonEnvironment fs (fun _ -> None))
        |> theOnlyWarning

    // Reported even though nothing was formatted, so a client can clear a warning it showed for
    // this file before the user added it to .fantomasignore.
    warning.FilePath |> should equal ignoredFile
    warning.Problems |> should be Empty

[<Test>]
let ``format nested ignored file`` () =
    runWithDaemon (fun client ->
        async {
            let sourceCode = "let foo = 4"

            use codeFile =
                new TemporaryFileCodeSample(
                    sourceCode,
                    fileName = "NicePrint",
                    subFolders = [| "src"; "Compiler"; "Checking" |]
                )

            use _ignoreFixture = new FantomasIgnoreFile("src/Compiler/Checking/NicePrint.fs")

            let request =
                {
                    SourceCode = sourceCode
                    FilePath = codeFile.Filename
                    Config = None
                    Cursor = None
                }

            let! response =
                client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                |> Async.AwaitTask

            match response with
            | FormatDocumentResponse.IgnoredFile _ -> Assert.Pass()
            | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
        }
    )

[<Test>]
let ``format cursor`` () =
    runWithDaemon (fun client ->
        async {
            let sourceCode =
                """
let a =
    "foobar"
"""

            use codeFile = new TemporaryFileCodeSample(sourceCode)

            let request =
                {
                    SourceCode = sourceCode
                    FilePath = codeFile.Filename
                    Config = None
                    Cursor = Some(FormatCursorPosition(3, 8))
                }

            let! response =
                client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                |> Async.AwaitTask

            match response with
            | FormatDocumentResponse.Formatted(cursor = Some cursor) ->
                Assert.AreEqual(1, cursor.Line)
                Assert.AreEqual(12, cursor.Column)
            | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
        }
    )
