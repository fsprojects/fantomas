module Fantomas.CoreGlobalTool.Tests.DaemonTests

open NUnit.Framework
open FsUnit
open Fantomas.CoreGlobalTool.Tests.TestHelpers
open Fantomas
open Fantomas.Client.Contracts
open Fantomas.Client.Service


[<Test>]
let ``spin up daemon and compare the version with the public api`` () =
    async {
        let processStart = getFantomasToolStartInfo "--daemon"
        use client = new LSPFantomasService(processStart)
        let! { Version = version } = (client :> FantomasService).Version None

        version
        |> should equal (CodeFormatter.GetVersion())
    }

[<Test>]
let ``spin up daemon and format`` () =
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

        response.Formatted
        |> String.normalizeNewLine
        |> should
            equal
            (String.normalizeNewLine
                "module Foobar
")
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
