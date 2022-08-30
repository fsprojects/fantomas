module Fantomas.Daemon

open System
open System.IO
open System.Threading.Tasks
open StreamJsonRpc
open Fantomas.Client.Contracts

type FantomasDaemon =
    interface IDisposable

    new: sender: Stream * reader: Stream -> FantomasDaemon

    [<JsonRpcMethod(Methods.Configuration)>]
    member Configuration: unit -> string

    [<JsonRpcMethod(Methods.FormatDocument, UseSingleObjectParameterDeserialization = true)>]
    member FormatDocumentAsync:
        request: FormatDocumentRequest -> Task<Client.LSPFantomasServiceTypes.FormatDocumentResponse>

    [<JsonRpcMethod(Methods.FormatSelection, UseSingleObjectParameterDeserialization = true)>]
    member FormatSelectionAsync:
        request: FormatSelectionRequest -> Task<Client.LSPFantomasServiceTypes.FormatSelectionResponse>

    [<JsonRpcMethod(Methods.Version)>]
    member Version: unit -> string

    /// returns a hot task that resolves when the stream has terminated
    member WaitForClose: Task
