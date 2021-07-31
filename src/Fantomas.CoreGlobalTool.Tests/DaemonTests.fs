module Fantomas.CoreGlobalTool.Tests.DaemonTests

open NUnit.Framework
open FsUnit
open Fantomas.CoreGlobalTool.Tests.TestHelpers
open Fantomas
open Fantomas.Client.Contracts
open Fantomas.Client.LSPFantomasService

let private assertFormatted (actual: string) (expected: string) : unit =
    String.normalizeNewLine actual
    |> should equal (String.normalizeNewLine expected)

let mutable client: FantomasService = Unchecked.defaultof<FantomasService>

[<SetUp>]
let ``create client`` () =
    let processStart = getFantomasToolStartInfo "--daemon"
    client <- new LSPFantomasService(processStart)

[<TearDown>]
let ``dispose client`` () = client.Dispose()

[<Test>]
let ``compare the version with the public api`` () =
    async {
        let! { Version = version } = client.VersionAsync()

        version
        |> should equal (CodeFormatter.GetVersion())
    }

[<Test>]
let ``format implementation file`` () =
    async {
        let sourceCode = "module Foobar"
        use codeFile = new TemporaryFileCodeSample(sourceCode)

        let request =
            { SourceCode = sourceCode
              FilePath = codeFile.Filename
              IsLastFile = false
              Config = None }

        let! response = client.FormatDocumentAsync(request)

        match response with
        | FormatDocumentResponse.Formatted (_, formatted) ->
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
              IsLastFile = false
              Config = None }

        let! response = client.FormatDocumentAsync(request)

        match response with
        | FormatDocumentResponse.Formatted (_, formatted) ->
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
              IsLastFile = false
              Config = None }

        let! response = client.FormatDocumentAsync(request)

        match response with
        | FormatDocumentResponse.Formatted (_, formatted) ->
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
              IsLastFile = false
              Config = Some(readOnlyDict [ "indent_size", "4" ]) }

        let! response = client.FormatDocumentAsync(request)

        match response with
        | FormatDocumentResponse.Formatted (_, formatted) ->
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
              IsLastFile = true
              Config = Some(readOnlyDict [ "end_of_line", "lf" ]) }

        let! response = client.FormatDocumentAsync(request)

        match response with
        | FormatDocumentResponse.Unchanged fileName -> fileName |> should equal codeFile.Filename
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
              IsLastFile = true
              Config = None }

        let! response = client.FormatDocumentAsync(request)

        match response with
        | FormatDocumentResponse.IgnoredFile fileName -> fileName |> should equal codeFile.Filename
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
              IsLastFile = false
              Config = None }

        let! response = client.FormatDocumentAsync(request)

        match response with
        | FormatDocumentResponse.Error (fileName, error) ->
            fileName |> should equal codeFile.Filename
            StringAssert.StartsWith("Parsing failed with errors:", error.Message)
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

        let! response = client.FormatSelectionAsync(request)

        match response with
        | FormatSelectionResponse.Formatted (fileName, formatted) ->
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
