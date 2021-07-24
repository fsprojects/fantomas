module Fantomas.CoreGlobalTool.Tests.DaemonTests

open NUnit.Framework
open FsUnit
open Fantomas.CoreGlobalTool.Tests.TestHelpers
open Fantomas
open Fantomas.Client.Contracts
open Fantomas.Client.Service

let private assertFormatted (response: FormatDocumentResponse) (expected: string) : unit =
    String.normalizeNewLine response.Formatted
    |> should equal (String.normalizeNewLine expected)

[<Test>]
let ``compare the version with the public api`` () =
    async {
        let processStart = getFantomasToolStartInfo "--daemon"
        use client = new LSPFantomasService(processStart)
        let! { Version = version } = (client :> FantomasService).Version None

        version
        |> should equal (CodeFormatter.GetVersion())
    }

[<Test>]
let ``format document`` () =
    async {
        let processStart = getFantomasToolStartInfo "--daemon"
        use client = new LSPFantomasService(processStart)
        let sourceCode = "module Foobar"
        use codeFile = new TemporaryFileCodeSample(sourceCode)

        let request =
            { SourceCode = sourceCode
              FilePath = codeFile.Filename
              Config = None }

        let! response =
            (client :> FantomasService)
                .FormatDocumentAsync(request, None)

        assertFormatted
            response
            "module Foobar
"
    }

[<Test>]
let ``format document respecting .editorconfig file`` () =
    async {
        let processStart = getFantomasToolStartInfo "--daemon"
        use client = new LSPFantomasService(processStart)
        let sourceCode = "module Foo\n\nlet a = //\n    4"
        use codeFile = new TemporaryFileCodeSample(sourceCode)

        use _config =
            new ConfigurationFile("[*.fs]\nindent_size=2")

        let request =
            { SourceCode = sourceCode
              FilePath = codeFile.Filename
              Config = None }

        let! response =
            (client :> FantomasService)
                .FormatDocumentAsync(request, None)

        assertFormatted
            response
            "module Foo

let a = //
  4
"
    }

[<Test>]
let ``custom configuration has precedence over .editorconfig file`` () =
    async {
        let processStart = getFantomasToolStartInfo "--daemon"
        use client = new LSPFantomasService(processStart)
        let sourceCode = "module Foo\n\nlet a = //\n    4"
        use codeFile = new TemporaryFileCodeSample(sourceCode)

        use _config =
            new ConfigurationFile("[*.fs]\nindent_size=2")

        let request =
            { SourceCode = sourceCode
              FilePath = codeFile.Filename
              Config = Some(readOnlyDict [ "indent_size", "4" ]) }

        let! response =
            (client :> FantomasService)
                .FormatDocumentAsync(request, None)

        assertFormatted
            response
            "module Foo

let a = //
    4
"
    }

[<Test>]
let ``find fantomas tool from working directory`` () =
    async {
        let filePath =
            @"C:\Users\nojaf\Projects\fantomas-tools\src\server\TriviaViewer\Encoders.fs"

        let originalCode = System.IO.File.ReadAllText(filePath)

        use client =
            new LSPFantomasService(@"C:\Users\nojaf\Projects\fantomas-tools")

        let! formattedResponse =
            (client :> FantomasService)
                .FormatDocumentAsync(
                    { SourceCode = originalCode
                      FilePath = filePath
                      Config = None },
                    None
                )

        let formattedCode = formattedResponse
        ()
    }
