module Fantomas.CoreGlobalTool.Tests.ToolLocatorTests

open Fantomas.Client
open Fantomas.Client.LSPFantomasServiceTypes
open Fantomas.Client.Contracts
open NUnit.Framework

[<Explicit "This is meant to troubleshoot local problems">]
[<Test>]
let ``locate fantomas tool`` () =
    let pwd = @"C:\Users\nojaf\Projects"
    let result = FantomasToolLocator.findFantomasTool (Folder pwd)

    match result with
    | Error error -> Assert.Fail $"Could not locate tool: %A{error}"
    | Ok(FantomasToolFound(FantomasVersion(version), startInfo)) ->
        let result = FantomasToolLocator.createFor startInfo

        match result with
        | Error error -> Assert.Fail $"Could not start tool: %A{error}"
        | Ok runningFantomasTool ->
            let version2 =
                runningFantomasTool.RpcClient.InvokeAsync<string>(Methods.Version).Result

            Assert.AreEqual(version, $"v{version2}")
