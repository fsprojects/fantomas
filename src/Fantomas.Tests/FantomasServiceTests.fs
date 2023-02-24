module Fantomas.CoreGlobalTool.Tests.FantomasServiceTests

open System
open System.IO
open Fantomas.Client.Contracts
open Fantomas.Client.LSPFantomasServiceTypes
open Fantomas.Client.LSPFantomasService
open NUnit.Framework

let toucheFileAndFormat (path: string) (service: FantomasService) : FantomasResponse =
    let content = File.ReadAllText path
    let dirtyContent = String.Concat(content, "    ")
    File.WriteAllText(path, dirtyContent)

    let request: FormatDocumentRequest =
        { SourceCode = dirtyContent
          FilePath = path
          Config = None
          Cursor = None }

    service.FormatDocumentAsync(request).Result

[<Explicit "This is meant to troubleshoot local problems">]
[<Test>]
let ``locate fantomas tool`` () =
    let service: FantomasService = new LSPFantomasService()

    let response =
        toucheFileAndFormat @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.Core\FormatConfig.fs" service

    Assert.AreEqual(int FantomasResponseCode.Formatted, response.Code)
