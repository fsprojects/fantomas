module Fantomas.Client.LSPFantomasService

open System.Diagnostics
open System.IO
open System.Threading
open StreamJsonRpc
open Fantomas.Client.Contracts
open Fantomas.Client.FantomasToolLocator

let private orDefaultCancellationToken =
    Option.defaultValue CancellationToken.None

type LSPFantomasService(daemonStartInfo: ProcessStartInfo) =

    do daemonStartInfo.RedirectStandardInput <- true
    do daemonStartInfo.RedirectStandardOutput <- true
    let daemonProcess = Process.Start daemonStartInfo

    let client =
        new JsonRpc(daemonProcess.StandardInput.BaseStream, daemonProcess.StandardOutput.BaseStream)

    do client.StartListening()

    interface FantomasService with
        member this.Dispose() =
            client.Dispose()
            daemonProcess.Dispose()

        member _.VersionAsync(?cancellationToken: CancellationToken) : Async<VersionResponse> =
            client.InvokeWithCancellationAsync<VersionResponse>(
                Methods.Version,
                cancellationToken = orDefaultCancellationToken cancellationToken
            )
            |> Async.AwaitTask

        member _.FormatDocumentAsync
            (
                formatDocumentOptions: FormatDocumentRequest,
                ?cancellationToken: CancellationToken
            ) : Async<FormatDocumentResponse> =
            client.InvokeWithParameterObjectAsync<FormatDocumentResponse>(
                Methods.FormatDocument,
                argument = formatDocumentOptions,
                cancellationToken = orDefaultCancellationToken cancellationToken
            )
            |> Async.AwaitTask

        member _.FormatSelectionAsync
            (
                formatSelectionRequest: FormatSelectionRequest,
                ?cancellationToken: CancellationToken
            ) =
            client.InvokeWithParameterObjectAsync<FormatSelectionResponse>(
                Methods.FormatSelection,
                argument = formatSelectionRequest,
                cancellationToken = orDefaultCancellationToken cancellationToken
            )
            |> Async.AwaitTask

        member _.ConfigurationAsync(?cancellationToken: CancellationToken) : Async<ConfigurationResponse> =
            client.InvokeWithCancellationAsync<ConfigurationResponse>(
                Methods.Configuration,
                cancellationToken = orDefaultCancellationToken cancellationToken
            )
            |> Async.AwaitTask

let createForWorkingDirectory (workingDirectory: string) : Result<LSPFantomasService, string> =
    if not (Directory.Exists workingDirectory) then
        raise (DirectoryNotFoundException(workingDirectory))
    else
        match findFantomasTool workingDirectory with
        | FoundLocalTool ->
            let processStart = ProcessStartInfo("dotnet")
            processStart.UseShellExecute <- false
            processStart.Arguments <- sprintf "fantomas --daemon"
            processStart.WorkingDirectory <- workingDirectory
            processStart.RedirectStandardOutput <- true
            processStart.RedirectStandardError <- true
            Ok(new LSPFantomasService(processStart))
        | FoundGlobalTool ->
            let processStart = ProcessStartInfo("fantomas")
            processStart.UseShellExecute <- false
            processStart.Arguments <- "--daemon"
            processStart.WorkingDirectory <- workingDirectory
            processStart.RedirectStandardOutput <- true
            processStart.RedirectStandardError <- true
            Ok(new LSPFantomasService(processStart))
        | NoCompatibleVersionFound ->
            // TODO: consider api choice here
            Error(sprintf "No compatible Fantomas version found in \"%s\"." workingDirectory)
