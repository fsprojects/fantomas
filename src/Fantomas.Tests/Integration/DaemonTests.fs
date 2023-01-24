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

let private runWithDaemon (fn: JsonRpc -> Async<unit>) =
    async {
        let struct (serverStream, clientStream) = FullDuplexStream.CreatePair()

        let daemon = new FantomasDaemon(serverStream, serverStream)

        let client = new JsonRpc(clientStream, clientStream)
        client.StartListening()
        do! fn client
        client.Dispose()
        (daemon :> IDisposable).Dispose()
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
            |> Seq.iter (fun setting -> Assert.True(config.Contains(setting)))
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
