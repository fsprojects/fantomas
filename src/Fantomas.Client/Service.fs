module Fantomas.Client.Service

open System.Diagnostics
open System.Threading
open StreamJsonRpc
open Fantomas.Client.Contracts

let private orDefaultCancellationToken =
    Option.defaultValue CancellationToken.None

type LSPFantomasService(daemonStartInfo: ProcessStartInfo) =

    do daemonStartInfo.RedirectStandardInput <- true
    do daemonStartInfo.RedirectStandardOutput <- true
    let daemonProcess = Process.Start daemonStartInfo

    let client =
        new JsonRpc(daemonProcess.StandardInput.BaseStream, daemonProcess.StandardOutput.BaseStream)

    do client.StartListening()

    new(workingDirectory: string) =
        let fantomasToolDll =
            @"C:\Users\nojaf\Projects\fantomas\src\Fantomas.CoreGlobalTool\bin\Release\netcoreapp3.1\fantomas-tool.dll"

        let processStart = ProcessStartInfo("dotnet")
        processStart.UseShellExecute <- false
        processStart.Arguments <- sprintf "%s --daemon" fantomasToolDll
        processStart.WorkingDirectory <- workingDirectory
        processStart.RedirectStandardOutput <- true
        processStart.RedirectStandardError <- true
        processStart.Arguments <- (sprintf "%s --daemon" fantomasToolDll)
        new LSPFantomasService(processStart)

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
                formatDocumentOptions: FormatDocumentOptions,
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

        member _.Configuration(?cancellationToken: CancellationToken) : Async<ConfigurationResult> =
            async {
                let ct =
                    orDefaultCancellationToken cancellationToken

                let! configurationResponse =
                    client.InvokeWithCancellationAsync<ConfigurationResult>(
                        Methods.Configuration,
                        cancellationToken = ct
                    )
                    |> Async.AwaitTask

                return configurationResponse
            }
