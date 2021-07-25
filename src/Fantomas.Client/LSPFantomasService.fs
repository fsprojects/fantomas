module Fantomas.Client.LSPFantomasService

open System.Diagnostics
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

        member _.Version(?cancellationToken: CancellationToken) : Async<VersionResponse> =
            async {
                let ct =
                    orDefaultCancellationToken cancellationToken

                return!
                    client.InvokeWithCancellationAsync<VersionResponse>(Methods.Version, cancellationToken = ct)
                    |> Async.AwaitTask
            }

        member _.FormatDocumentAsync
            (
                formatDocumentOptions: FormatDocumentRequest,
                ?cancellationToken: CancellationToken
            ) : Async<FormatDocumentResponse> =
            async {
                let ct =
                    orDefaultCancellationToken cancellationToken

                let! formatDocumentResponse =
                    client.InvokeWithParameterObjectAsync<FormatDocumentResponse>(
                        Methods.FormatDocument,
                        argument = formatDocumentOptions,
                        cancellationToken = ct
                    )
                    |> Async.AwaitTask

                return formatDocumentResponse
            }

        member _.Configuration(?cancellationToken: CancellationToken) : Async<ConfigurationResponse> =
            async {
                let ct =
                    orDefaultCancellationToken cancellationToken

                let! configurationResponse =
                    client.InvokeWithCancellationAsync<ConfigurationResponse>(
                        Methods.Configuration,
                        cancellationToken = ct
                    )
                    |> Async.AwaitTask

                return configurationResponse
            }

let createForWorkingDirectory (workingDirectory: string) : Result<LSPFantomasService, string> =
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
