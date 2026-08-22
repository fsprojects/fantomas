module Fantomas.Daemon

open System
open System.IO
open System.Threading.Tasks
open StreamJsonRpc
open System.IO.Abstractions
open Serilog
open Fantomas.Core
open Fantomas.EditorConfig
open Fantomas.Client.Contracts

/// How the daemon reaches the world outside itself. Narrower than the command line tool's
/// environment: the daemon looks for an ignore file per request rather than resolving one for the
/// run, and it draws nothing, so it needs neither a resolved ignore file nor a console.
[<NoComparison; NoEquality>]
type DaemonEnvironment =
    {
        FileSystem: IFileSystem
        /// The `.editorconfig` result for a file, problems and all. Wider than the command line
        /// tool's hook, which only needs the configuration: the daemon sends what it could not use
        /// to its client rather than warning about it, so it needs the problems too.
        ReadConfiguration: string -> EditorConfigResult option
        Log: ILogger
    }

type FantomasDaemon =
    interface IDisposable

    new: sender: Stream * reader: Stream * environment: DaemonEnvironment -> FantomasDaemon

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
