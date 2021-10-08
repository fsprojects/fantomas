module Fantomas.Client.LSPFantomasService

open System.Diagnostics
open System.Threading
open System.Threading.Tasks
open StreamJsonRpc
open Fantomas.Client.Contracts
open Fantomas.Client.LSPFantomasServiceTypes

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

        member _.VersionAsync(?cancellationToken: CancellationToken) : Task<VersionResponse> =
            client.InvokeWithCancellationAsync<VersionResponse>(
                Methods.Version,
                cancellationToken = orDefaultCancellationToken cancellationToken
            )

        member _.FormatDocumentAsync
            (
                formatDocumentOptions: FormatDocumentRequest,
                ?cancellationToken: CancellationToken
            ) : Task<FormatResponse> =
            client
                .InvokeWithParameterObjectAsync<FormatDocumentResponse>(
                    Methods.FormatDocument,
                    argument = formatDocumentOptions,
                    cancellationToken = orDefaultCancellationToken cancellationToken
                )
                .ContinueWith(fun (t: Task<FormatDocumentResponse>) -> t.Result.AsFormatResponse())

        member _.FormatSelectionAsync
            (
                formatSelectionRequest: FormatSelectionRequest,
                ?cancellationToken: CancellationToken
            ) =
            client
                .InvokeWithParameterObjectAsync<FormatSelectionResponse>(
                    Methods.FormatSelection,
                    argument = formatSelectionRequest,
                    cancellationToken = orDefaultCancellationToken cancellationToken
                )
                .ContinueWith(fun (t: Task<FormatSelectionResponse>) -> t.Result.AsFormatResponse())

        member _.ConfigurationAsync(?cancellationToken: CancellationToken) : Task<ConfigurationResponse> =
            client.InvokeWithCancellationAsync<ConfigurationResponse>(
                Methods.Configuration,
                cancellationToken = orDefaultCancellationToken cancellationToken
            )
