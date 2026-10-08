module FantomasClientTests.DisposeTests

open System
open System.Threading.Tasks
open NUnit.Framework
open Fantomas.Client.Contracts
open Fantomas.Client.LSPFantomasService
open Fantomas.Client.LSPFantomasServiceTypes

// Run on another thread and waited on with a timeout, because the failure this guards against is a
// `PostAndReply` that never returns, which would otherwise hang the test run rather than fail it.
let private completesInTime (action: unit -> unit) : bool =
    Task.Run(Action action).Wait(TimeSpan.FromSeconds 10.)

[<Test>]
let ``clearing the cache of a disposed service returns`` () =
    let service: FantomasService = new LSPFantomasService()
    service.Dispose()

    Assert.That(completesInTime service.ClearCache, Is.True)

[<Test>]
let ``a request to a disposed service is answered as cancelled`` () =
    let service: FantomasService = new LSPFantomasService()
    service.Dispose()

    // The service is disposed before the path is looked at, so the file need not exist.
    let response: FantomasResponse =
        service.VersionAsync(IO.Path.Combine(IO.Path.GetTempPath(), "Disposed.fs")).Result

    Assert.That(response.Code, Is.EqualTo(int FantomasResponseCode.CancellationWasRequested))
