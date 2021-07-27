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

[<Test>]
let ``compare the version with the public api`` () =
    async {
        let processStart = getFantomasToolStartInfo "--daemon"
        use client = new LSPFantomasService(processStart)
        let! { Version = version } = (client :> FantomasService).VersionAsync()

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
                .FormatDocumentAsync(request)

        assertFormatted
            response.Formatted
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
                .FormatDocumentAsync(request)

        assertFormatted
            response.Formatted
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
                .FormatDocumentAsync(request)

        assertFormatted
            response.Formatted
            "module Foo

let a = //
    4
"
    }

[<Test>]
let ``format selection`` () =
    async {
        let sourceCode =
            """module Foo

let    x     = 4
let    y     = 5
"""

        let processStart = getFantomasToolStartInfo "--daemon"
        use client = new LSPFantomasService(processStart)
        use _codeFile = new TemporaryFileCodeSample(sourceCode)

        let request: FormatSelectionRequest =
            let range = FormatSelectionRange(3, 0, 3, 16)

            { SourceCode = sourceCode
              FilePath = "tmp.fsx" // codeFile.Filename
              Config = None
              Range = range }

        let! response =
            (client :> FantomasService)
                .FormatSelectionAsync(request)

        assertFormatted response.Formatted "let x = 4\n"
    }


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
