module Fantomas.Tests.Integration.DaemonTests

open System
open Fantomas.Client.LSPFantomasServiceTypes
open Fantomas.Daemon
open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelpers
open Fantomas.Core
open Fantomas.Client.Contracts
open Nerdbank.Streams
open StreamJsonRpc

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

/// As `runWithDaemon`, but listening for the configuration warnings the daemon pushes.
/// The handler has to be attached before the client starts listening.
let private runWithDaemonCollectingWarnings (fn: JsonRpc -> Async<unit>) : Async<ConfigurationWarning list> =
    async {
        let struct (serverStream, clientStream) = FullDuplexStream.CreatePair()

        let daemon = new FantomasDaemon(serverStream, serverStream, realDaemonEnvironment)

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
        let maxAttempts = 50

        let rec waitForWarning (attempt: int) : Async<unit> =
            async {
                let arrived = lock warnings (fun () -> warnings.Count > 0)

                if arrived || attempt >= maxAttempts then
                    return ()
                else
                    do! Async.Sleep 100
                    return! waitForWarning (attempt + 1)
            }

        try
            do! fn client
            do! waitForWarning 0
        finally
            // A failing assertion must not leave the connection and the daemon behind.
            client.Dispose()
            (daemon :> IDisposable).Dispose()

        return List.ofSeq warnings
    }

[<Test>]
let ``version request`` () =
    runWithDaemon (fun client ->
        async {
            let! version = client.InvokeAsync<string>(Methods.Version) |> Async.AwaitTask
            version |> should equal (CodeFormatter.GetVersion())
        })

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
                Assert.That(config.Contains(setting), Is.True, $"Setting %s{setting} not found"))
        })

[<Test>]
let ``format implementation file`` () =
    runWithDaemon (fun client ->
        async {
            let sourceCode = "module   Foobar"
            use codeFile = new TemporaryFileCodeSample(sourceCode)

            let request =
                { SourceCode = sourceCode
                  FilePath = codeFile.Filename
                  Config = None
                  Cursor = None }

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
        })

[<Test>]
let ``format implementation file, unchanged`` () =
    runWithDaemon (fun client ->
        async {
            let sourceCode = "module Foobar\n"
            use codeFile = new TemporaryFileCodeSample(sourceCode)

            let request =
                { SourceCode = sourceCode
                  FilePath = codeFile.Filename
                  Config = Some(readOnlyDict [ "end_of_line", "lf" ])
                  Cursor = None }

            let! response =
                client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                |> Async.AwaitTask

            match response with
            | FormatDocumentResponse.Unchanged _ -> Assert.Pass()
            | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
        })

[<Test>]
let ``format implementation file, error`` () =
    runWithDaemon (fun client ->
        async {
            let sourceCode = "let foo ="
            use codeFile = new TemporaryFileCodeSample(sourceCode)

            let request =
                { SourceCode = sourceCode
                  FilePath = codeFile.Filename
                  Config = None
                  Cursor = None }

            let! response =
                client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                |> Async.AwaitTask

            match response with
            | FormatDocumentResponse.Error _ -> Assert.Pass()
            | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
        })

[<Test>]
let ``format implementation file, ignored file`` () =
    runWithDaemon (fun client ->
        async {
            let sourceCode = "let foo = 4"
            use codeFile = new TemporaryFileCodeSample(sourceCode)
            use _ignoreFixture = new FantomasIgnoreFile("*.fs")

            let request =
                { SourceCode = sourceCode
                  FilePath = codeFile.Filename
                  Config = None
                  Cursor = None }

            let! response =
                client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                |> Async.AwaitTask

            match response with
            | FormatDocumentResponse.IgnoredFile _ -> Assert.Pass()
            | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
        })

[<Test>]
let ``format signature file`` () =
    runWithDaemon (fun client ->
        async {
            let sourceCode = "module Foobar\n\nval meh :  int"

            use codeFile = new TemporaryFileCodeSample(sourceCode, extension = "fsi")

            let request =
                { SourceCode = sourceCode
                  FilePath = codeFile.Filename
                  Config = None
                  Cursor = None }

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
        })

[<Test>]
let ``format document respecting .editorconfig file`` () =
    runWithDaemon (fun client ->
        async {
            let sourceCode = "module Foo\n\nlet a = //\n    4"
            use codeFile = new TemporaryFileCodeSample(sourceCode)

            use _config = new ConfigurationFile("[*.fs]\nindent_size=2")

            let request =
                { SourceCode = sourceCode
                  FilePath = codeFile.Filename
                  Config = None
                  Cursor = None }

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
        })

[<Test>]
let ``custom configuration has precedence over .editorconfig file`` () =
    runWithDaemon (fun client ->
        async {
            let sourceCode = "module Foo\n\nlet a = //\n    4"
            use codeFile = new TemporaryFileCodeSample(sourceCode)

            use _config = new ConfigurationFile("[*.fs]\nindent_size=2")

            let request =
                { SourceCode = sourceCode
                  FilePath = codeFile.Filename
                  Config = Some(readOnlyDict [ "indent_size", "4" ])
                  Cursor = None }

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
        })

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

                { SourceCode = sourceCode
                  FilePath = codeFile.Filename
                  Config = None
                  Range = range }

            let! response =
                client.InvokeAsync<FormatSelectionResponse>(Methods.FormatSelection, request)
                |> Async.AwaitTask

            match response with
            | FormatSelectionResponse.Formatted(fileName, formatted, _) ->
                fileName |> should equal codeFile.Filename
                assertFormatted formatted "let x = 4"
            | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
        })

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

                { SourceCode = sourceCode
                  FilePath = codeFile.Filename
                  Config = None
                  Range = range }

            let! response =
                client.InvokeAsync<FormatSelectionResponse>(Methods.FormatSelection, request)
                |> Async.AwaitTask

            match response with
            | FormatSelectionResponse.Formatted(fileName, formatted, _) ->
                fileName |> should equal codeFile.Filename
                assertFormatted formatted "val x: int"
            | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
        })

[<Test>]
let ``format document with both .editorconfig file and custom config`` () =
    runWithDaemon (fun client ->
        async {
            let sourceCode = "module Foo\n\nlet add (a:int) (b:int) = //\n    a + b"

            use codeFile = new TemporaryFileCodeSample(sourceCode)

            use _config = new ConfigurationFile("[*.fs]\nindent_size=2")

            let request =
                { SourceCode = sourceCode
                  FilePath = codeFile.Filename
                  Config = Some(readOnlyDict [ "fsharp_space_before_colon", "true" ])
                  Cursor = None }

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
        })

[<Test>]
let ``a configuration warning is sent for settings the daemon cannot use`` () =
    let sourceCode = "module Foo\n\nlet add a  b = a + b"
    use codeFile = new TemporaryFileCodeSample(sourceCode)

    let warnings =
        runWithDaemonCollectingWarnings (fun client ->
            async {
                let request =
                    { SourceCode = sourceCode
                      FilePath = codeFile.Filename
                      Config =
                        Some(
                            readOnlyDict [ "fsharp_bogus_option", "true"; "fsharp_experimental_elmish", "not_a_bool" ]
                        )
                      Cursor = None }

                let! _response =
                    client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                    |> Async.AwaitTask

                return ()
            })
        |> Async.RunSynchronously

    match warnings with
    | [ warning ] ->
        warning.FilePath |> should equal codeFile.Filename

        warning.Problems
        |> Array.map (fun problem -> problem.Code, problem.Source, problem.Setting, problem.Value)
        |> should
            equal
            [| int ConfigurationProblemCode.UnknownSetting,
               int ConfigurationProblemSource.Request,
               "fsharp_bogus_option",
               null
               int ConfigurationProblemCode.UnrecognizedValue,
               int ConfigurationProblemSource.Request,
               "fsharp_experimental_elmish",
               "not_a_bool" |]
    | otherwise -> Assert.Fail $"Expected exactly one configuration warning, got %A{otherwise}"

[<Test>]
let ``a configuration warning with no problems is sent when the configuration is fine`` () =
    let sourceCode = "module Foo\n\nlet add a  b = a + b"
    use codeFile = new TemporaryFileCodeSample(sourceCode)

    let warnings =
        runWithDaemonCollectingWarnings (fun client ->
            async {
                let request =
                    { SourceCode = sourceCode
                      FilePath = codeFile.Filename
                      Config = Some(readOnlyDict [ "fsharp_space_before_colon", "true" ])
                      Cursor = None }

                let! _response =
                    client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                    |> Async.AwaitTask

                return ()
            })
        |> Async.RunSynchronously

    // Sent even when there is nothing wrong, so a client can clear what it reported earlier.
    match warnings with
    | [ warning ] -> warning.Problems |> should be Empty
    | otherwise -> Assert.Fail $"Expected exactly one configuration warning, got %A{otherwise}"

// The settings an editor sends are checked the same way the ones on disk are. A mistake in them
// must not disturb the response: in daemon mode standard out carries the JSON-RPC protocol, so a
// warning that reached it would fault the connection rather than merely be noisy.
[<Test>]
let ``format document with a custom config the daemon cannot use`` () =
    runWithDaemon (fun client ->
        async {
            let sourceCode = "module Foo\n\nlet add a  b = a + b"

            use codeFile = new TemporaryFileCodeSample(sourceCode)

            let request =
                { SourceCode = sourceCode
                  FilePath = codeFile.Filename
                  Config =
                    Some(readOnlyDict [ "fsharp_bogus_option", "true"; "fsharp_experimental_elmish", "not_a_bool" ])
                  Cursor = None }

            let! response =
                client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                |> Async.AwaitTask

            match response with
            | FormatDocumentResponse.Formatted(formattedContent = formatted) ->
                assertFormatted formatted "module Foo\n\nlet add a b = a + b\n"
            | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
        })

[<Test>]
let ``settings from an editorconfig on disk are reported with their source and files`` () =
    let sourceCode = "module Foo\n\nlet add a  b = a + b"
    use codeFile = new TemporaryFileCodeSample(sourceCode)

    use _config =
        new ConfigurationFile(
            """
[*.fs]
fsharp_bogus_option = true
fsharp_experimental_elmish = not_a_bool
"""
        )

    let warnings =
        runWithDaemonCollectingWarnings (fun client ->
            async {
                let request =
                    { SourceCode = sourceCode
                      FilePath = codeFile.Filename
                      Config = None
                      Cursor = None }

                let! _response =
                    client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                    |> Async.AwaitTask

                return ()
            })
        |> Async.RunSynchronously

    match warnings with
    | [ warning ] ->
        warning.Problems
        |> Array.map (fun problem -> problem.Code, problem.Source, problem.Setting, problem.Value)
        |> should
            equal
            [| int ConfigurationProblemCode.UnknownSetting,
               int ConfigurationProblemSource.EditorConfig,
               "fsharp_bogus_option",
               null
               int ConfigurationProblemCode.UnrecognizedValue,
               int ConfigurationProblemSource.EditorConfig,
               "fsharp_experimental_elmish",
               "not_a_bool" |]

        // The paths of the files that contributed, so a client can point the user at them.
        Assert.That(warning.EditorConfigFiles, Is.Not.Empty)

        Assert.That(
            warning.EditorConfigFiles
            |> Array.forall (fun file ->
                System.IO.Path.IsPathRooted file
                && file.EndsWith(".editorconfig", System.StringComparison.Ordinal)),
            Is.True,
            $"Expected absolute .editorconfig paths, got %A{warning.EditorConfigFiles}"
        )

        // Not just any .editorconfig: the one this test wrote.
        Assert.That(warning.EditorConfigFiles, Does.Contain _config.Filename)
    | otherwise -> Assert.Fail $"Expected exactly one configuration warning, got %A{otherwise}"

[<Test>]
let ``format selection reports configuration warnings too`` () =
    let sourceCode = "module Foo\n\nlet add a  b = a + b"
    use codeFile = new TemporaryFileCodeSample(sourceCode)

    let warnings =
        runWithDaemonCollectingWarnings (fun client ->
            async {
                let request =
                    { SourceCode = sourceCode
                      FilePath = codeFile.Filename
                      Config = Some(readOnlyDict [ "fsharp_bogus_option", "true" ])
                      Range = FormatSelectionRange(3, 0, 3, 19) }

                let! _response =
                    client.InvokeAsync<FormatSelectionResponse>(Methods.FormatSelection, request)
                    |> Async.AwaitTask

                return ()
            })
        |> Async.RunSynchronously

    match warnings with
    | [ warning ] ->
        warning.Problems
        |> Array.map (fun problem -> problem.Code, problem.Setting)
        |> should equal [| int ConfigurationProblemCode.UnknownSetting, "fsharp_bogus_option" |]
    | otherwise -> Assert.Fail $"Expected exactly one configuration warning, got %A{otherwise}"

[<Test>]
let ``an ignored file still reports an empty configuration warning`` () =
    let sourceCode = "let foo = 4"

    use codeFile =
        new TemporaryFileCodeSample(sourceCode, fileName = "Ignored", subFolders = [| "warned" |])

    use _ignoreFixture = new FantomasIgnoreFile("warned/Ignored.fs")

    let warnings =
        runWithDaemonCollectingWarnings (fun client ->
            async {
                let request =
                    { SourceCode = sourceCode
                      FilePath = codeFile.Filename
                      Config = None
                      Cursor = None }

                let! _response =
                    client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                    |> Async.AwaitTask

                return ()
            })
        |> Async.RunSynchronously

    // Reported even though nothing was formatted, so a client can clear a warning it showed for
    // this file before the user added it to .fantomasignore.
    match warnings with
    | [ warning ] ->
        warning.FilePath |> should equal codeFile.Filename
        warning.Problems |> should be Empty
    | otherwise -> Assert.Fail $"Expected exactly one configuration warning, got %A{otherwise}"

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
                { SourceCode = sourceCode
                  FilePath = codeFile.Filename
                  Config = None
                  Cursor = None }

            let! response =
                client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                |> Async.AwaitTask

            match response with
            | FormatDocumentResponse.IgnoredFile _ -> Assert.Pass()
            | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
        })

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
                { SourceCode = sourceCode
                  FilePath = codeFile.Filename
                  Config = None
                  Cursor = Some(FormatCursorPosition(3, 8)) }

            let! response =
                client.InvokeAsync<FormatDocumentResponse>(Methods.FormatDocument, request)
                |> Async.AwaitTask

            match response with
            | FormatDocumentResponse.Formatted(cursor = Some cursor) ->
                Assert.AreEqual(1, cursor.Line)
                Assert.AreEqual(12, cursor.Column)
            | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
        })
