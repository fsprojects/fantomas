module Fantomas.CoreGlobalTool.Tests.DaemonTests

open System
open Fantomas.Client.LSPFantomasServiceTypes
open Fantomas.CoreGlobalTool.Daemon
open NUnit.Framework
open FsUnit
open Fantomas.CoreGlobalTool.Tests.TestHelpers
open Fantomas
open Fantomas.Client.Contracts
open Fantomas.Client.LSPFantomasService
open Nerdbank.Streams
open StreamJsonRpc

let private assertFormatted (actual: string) (expected: string) : unit =
    String.normalizeNewLine actual
    |> should equal (String.normalizeNewLine expected)

let mutable service: FantomasService = Unchecked.defaultof<FantomasService>

[<SetUp>]
let ``create service`` () = service <- new LSPFantomasService()

[<TearDown>]
let ``dispose service`` () = service.Dispose()

[<Test>]
let ``compare the version with the public api`` () =
    async {
        let! { Content = version } =
            service.VersionAsync(@"C:\Users\fverdonck\Temp\meh\SomeFile.fs")
            |> Async.AwaitTask

        version
        |> Option.defaultValue "???"
        |> should equal (CodeFormatter.GetVersion())
    }

[<Test>]
let ``cached version`` () =
    async {
        let! _ =
            service.VersionAsync(@"C:\Users\fverdonck\Temp\meh\SomeFile.fs")
            |> Async.AwaitTask

        let! { Content = version } =
            service.VersionAsync(@"C:\Users\fverdonck\Temp\meh\SomeFile.fs")
            |> Async.AwaitTask

        version
        |> Option.defaultValue "???"
        |> should equal (CodeFormatter.GetVersion())
    }

[<Test>]
let ``relative path should not be accepted`` () =
    async {
        let! { Code = code } =
            service.ConfigurationAsync @"..\src\Fantomas\CodePrinter.fs"
            |> Async.AwaitTask

        code
        |> should equal (int FantomasResponseCode.FilePathIsNotAbsolute)
    }

[<Test>]
let ``fantomas tool file`` () =
    async {
        let path =
            @"c:\Users\fverdonck\Projects\fantomas-tools\src\server\ASTViewer\Decoders.fs"

        let source = System.IO.File.ReadAllText path

        let! { Code = code } =
            service.FormatDocumentAsync
                { FilePath = path
                  SourceCode = source
                  Config = None }
            |> Async.AwaitTask

        code
        |> should equal (int FantomasResponseCode.ToolNotFound)
    }

[<Test>]
let ``config as json`` () =
    async {
        let! { Content = json } =
            service.ConfigurationAsync @"C:\Users\fverdonck\Temp\meh\SomeFile.fs"
            |> Async.AwaitTask

        match json with
        | Some json -> json.StartsWith("{") |> should equal true
        | None -> Assert.Fail "expected json config"
    }

[<Test>]
let ``version request`` () =
    async {
        let struct (serverStream, clientStream) = FullDuplexStream.CreatePair()

        let daemon =
            new FantomasDaemon(serverStream, serverStream)

        let client = new JsonRpc(clientStream, clientStream)
        client.StartListening()

        let! version =
            client.InvokeAsync<string>(Methods.Version)
            |> Async.AwaitTask

        version
        |> should equal (CodeFormatter.GetVersion())

        client.Dispose()
        (daemon :> IDisposable).Dispose()
    }

[<Test>]
let ``should respect editorconfig`` () =
    async {
        let struct (serverStream, clientStream) = FullDuplexStream.CreatePair()

        let _daemon =
            new FantomasDaemon(serverStream, serverStream)

        let client = new JsonRpc(clientStream, clientStream)
        client.StartListening()

        let path =
            @"c:\Users\fverdonck\Projects\fantomas-tools\src\server\ASTViewer\Decoders.fs"

        let source = System.IO.File.ReadAllText path

        let req =
            { FilePath = path
              SourceCode = source
              Config = None }

        let! res =
            client.InvokeAsync<FantomasResponse>(Methods.FormatDocument, req)
            |> Async.AwaitTask

        res.Code |> should equal 1
    }

[<Test>]
let ``format implementation file`` () =
    async {
        let sourceCode = "module Foobar"
        use codeFile = new TemporaryFileCodeSample(sourceCode)

        let request =
            { SourceCode = sourceCode
              FilePath = codeFile.Filename
              Config = None }

        let! response =
            service.FormatDocumentAsync(request)
            |> Async.AwaitTask

        match response with
        | { Code = 1; Content = Some formatted } ->
            assertFormatted
                formatted
                "module Foobar
"
        | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
    }

[<Test>]
let ``format signature file`` () =
    async {
        let sourceCode = "module Foobar\n\nval meh :  int"

        use codeFile =
            new TemporaryFileCodeSample(sourceCode, extension = "fsi")

        let request =
            { SourceCode = sourceCode
              FilePath = codeFile.Filename
              Config = None }

        let! response =
            service.FormatDocumentAsync(request)
            |> Async.AwaitTask

        match response with
        | { Code = 1; Content = Some formatted } ->
            assertFormatted
                formatted
                "module Foobar

val meh : int
"
        | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
    }

[<Test>]
let ``format document respecting .editorconfig file`` () =
    async {
        let sourceCode = "module Foo\n\nlet a = //\n    4"
        use codeFile = new TemporaryFileCodeSample(sourceCode)

        use _config =
            new ConfigurationFile("[*.fs]\nindent_size=2")

        let request =
            { SourceCode = sourceCode
              FilePath = codeFile.Filename
              Config = None }

        let! response =
            service.FormatDocumentAsync(request)
            |> Async.AwaitTask

        match response with
        | { Code = 1; Content = Some formatted } ->
            assertFormatted
                formatted
                "module Foo

let a = //
  4
"
        | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
    }

[<Test>]
let ``custom configuration has precedence over .editorconfig file`` () =
    async {
        let sourceCode = "module Foo\n\nlet a = //\n    4"
        use codeFile = new TemporaryFileCodeSample(sourceCode)

        use _config =
            new ConfigurationFile("[*.fs]\nindent_size=2")

        let request =
            { SourceCode = sourceCode
              FilePath = codeFile.Filename
              Config = Some(readOnlyDict [ "indent_size", "4" ]) }

        let! response =
            service.FormatDocumentAsync(request)
            |> Async.AwaitTask

        match response with
        | { Code = 1; Content = Some formatted } ->
            assertFormatted
                formatted
                "module Foo

let a = //
    4
"
        | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
    }

[<Test>]
let ``already formatted file returns unchanged`` () =
    async {
        let sourceCode = "let a = x\n"

        use codeFile =
            new TemporaryFileCodeSample(sourceCode, extension = "fsx")

        let request =
            { SourceCode = sourceCode
              FilePath = codeFile.Filename
              Config = Some(readOnlyDict [ "end_of_line", "lf" ]) }

        let! response =
            service.FormatDocumentAsync(request)
            |> Async.AwaitTask

        match response with
        | { Code = 2; FilePath = fileName } -> fileName |> should equal codeFile.Filename
        | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
    }

[<Test>]
let ``ignored file returns ignored`` () =
    async {
        let sourceCode = "let a   =   x\n"

        use codeFile =
            new TemporaryFileCodeSample(sourceCode, extension = "fsx")

        use _ignoreFile = new FantomasIgnoreFile("*.fsx")

        let request =
            { SourceCode = sourceCode
              FilePath = codeFile.Filename
              Config = None }

        let! response =
            service.FormatDocumentAsync(request)
            |> Async.AwaitTask

        match response with
        | { Code = 4; FilePath = fileName } -> fileName |> should equal codeFile.Filename
        | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
    }

[<Test>]
let ``format invalid code`` () =
    async {
        let sourceCode = "module Foobar\n\nlet ziggy ="
        use codeFile = new TemporaryFileCodeSample(sourceCode)

        let request =
            { SourceCode = sourceCode
              FilePath = codeFile.Filename
              Config = None }

        let! response =
            service.FormatDocumentAsync(request)
            |> Async.AwaitTask

        match response with
        | { Code = 3
            Content = Some error
            FilePath = fileName } ->
            fileName |> should equal codeFile.Filename
            StringAssert.StartsWith("Parsing failed with errors:", error)
        | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
    }

[<Test>]
let ``format selection`` () =
    async {
        let sourceCode =
            """module Foo

let    x     = 4
let    y     = 5
"""

        use _codeFile = new TemporaryFileCodeSample(sourceCode)

        let request: FormatSelectionRequest =
            let range = FormatSelectionRange(3, 0, 3, 16)

            { SourceCode = sourceCode
              FilePath = "tmp.fsx" // codeFile.Filename
              Config = None
              Range = range }

        let! response =
            service.FormatSelectionAsync(request)
            |> Async.AwaitTask

        match response with
        | { Code = 1
            Content = Some formatted
            FilePath = fileName } ->
            fileName |> should equal "tmp.fsx"
            assertFormatted formatted "let x = 4\n"
        | otherResponse -> Assert.Fail $"Unexpected response %A{otherResponse}"
    }

(*
[<Test>]
let ``find fantomas tool from working directory`` () =
    async {
        let filePath =
            @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Client\LSPFantomasService.fs"

        let originalCode = System.IO.File.ReadAllText(filePath)

        let workingDir = @"C:\Users\nojaf\Projects\fantomas"

        use client =
            let x = createForWorkingDirectory workingDir

            match x with
            | Ok service -> service
            | Error error -> failwithf "butter: %s" error

        let! formattedResponse =
            (client :> FantomasService)
                .FormatDocumentAsync(
                    { SourceCode = originalCode
                      FilePath = filePath
                      Config = None }
                )

        let formattedCode = formattedResponse
        ()
    }
*)
