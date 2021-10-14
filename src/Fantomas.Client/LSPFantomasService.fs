module Fantomas.Client.LSPFantomasService

open System.IO
open System.Threading
open System.Threading.Tasks
open StreamJsonRpc
open Fantomas.Client.Contracts
open Fantomas.Client.LSPFantomasServiceTypes
open Fantomas.Client.FantomasToolLocator

let private orDefaultCancellationToken =
    Option.defaultValue CancellationToken.None

type Msg =
    | GetDaemon of Folder * AsyncReplyChannel<JsonRpc option>
    | Shutdown

let private agent =
    MailboxProcessor.Start
        (fun inbox ->
            let rec messageLoop (state: ServiceState) =
                async {
                    let! msg = inbox.Receive()

                    let nextState =
                        match msg with
                        | GetDaemon (folder, replyChannel) ->
                            // get the version of that folder
                            // look in the cache first
                            let versionFromCache = Map.tryFind folder state.FolderToVersion

                            match versionFromCache with
                            | Some version ->
                                let daemon = Map.tryFind version state.Daemons

                                match daemon with
                                | Some _ ->
                                    replyChannel.Reply daemon

                                    { state with
                                          FolderToVersion = Map.add folder version state.FolderToVersion }
                                | None ->
                                    // This is a strange situation, we know what version is linked to that folder but there is no daemon
                                    // The moment a version is added, is also the moment a daemon is re-used or created
                                    replyChannel.Reply None
                                    state
                            | None ->
                                let version = findFantomasTool folder

                                match version with
                                | FantomasToolResult.NoCompatibleVersionFound ->
                                    // No compatible version was found
                                    replyChannel.Reply None
                                    state

                                | FantomasToolResult.FoundLocalTool (_, version) ->
                                    match Map.tryFind version state.Daemons with
                                    | Some daemon ->
                                        replyChannel.Reply(Some daemon)
                                        state
                                    | None ->
                                        let daemon = createForWorkingDirectory folder false
                                        replyChannel.Reply(Some daemon)

                                        { state with
                                              Daemons = Map.add version daemon state.Daemons
                                              FolderToVersion = Map.add folder version state.FolderToVersion }

                                | FantomasToolResult.FoundGlobalTool (_, version) ->
                                    match Map.tryFind version state.Daemons with
                                    | Some daemon ->
                                        replyChannel.Reply(Some daemon)
                                        state
                                    | None ->
                                        let daemon = createForWorkingDirectory folder true
                                        replyChannel.Reply(Some daemon)

                                        { state with
                                              Daemons = Map.add version daemon state.Daemons
                                              FolderToVersion = Map.add folder version state.FolderToVersion }


                        | Shutdown ->
                            Map.toList state.Daemons
                            |> List.iter (fun (_, daemon) -> daemon.Dispose())

                            ServiceState.Empty

                    return! messageLoop nextState
                }

            messageLoop ServiceState.Empty)

let private getDaemon (folder: Folder) : JsonRpc option =
    agent.PostAndReply(fun replyChannel -> GetDaemon(folder, replyChannel))

let private daemonNotFoundResponse filePath : Task<FantomasResponse> =
    { Code = int FantomasResponseCode.ToolNotFound
      FilePath = filePath
      Content = Some(sprintf "No fantomas tool was found for \"%s\"" filePath) }
    |> Task.FromResult

type LSPFantomasService() =

    interface FantomasService with
        member this.Dispose() = agent.Post Shutdown

        member _.VersionAsync(filePath, ?cancellationToken: CancellationToken) : Task<FantomasResponse> =
            let folder = Path.GetDirectoryName filePath |> Folder

            match getDaemon folder with
            | None -> daemonNotFoundResponse filePath
            | Some client ->
                client
                    .InvokeWithCancellationAsync<string>(
                        Methods.Version,
                        cancellationToken = orDefaultCancellationToken cancellationToken
                    )
                    .ContinueWith(fun (t: Task<string>) ->
                        { Code = int FantomasResponseCode.Version
                          Content = Some t.Result
                          FilePath = filePath })

        member _.FormatDocumentAsync
            (
                formatDocumentOptions: FormatDocumentRequest,
                ?cancellationToken: CancellationToken
            ) : Task<FantomasResponse> =
            failwith "todo"
        //            client
//                .InvokeWithParameterObjectAsync<FormatDocumentResponse>(
//                    Methods.FormatDocument,
//                    argument = formatDocumentOptions,
//                    cancellationToken = orDefaultCancellationToken cancellationToken
//                )
//                .ContinueWith(fun (t: Task<FormatDocumentResponse>) -> t.Result.AsFormatResponse())

        member _.FormatSelectionAsync
            (
                formatSelectionRequest: FormatSelectionRequest,
                ?cancellationToken: CancellationToken
            ) =
            failwith "todo"
        //            client
//                .InvokeWithParameterObjectAsync<FormatSelectionResponse>(
//                    Methods.FormatSelection,
//                    argument = formatSelectionRequest,
//                    cancellationToken = orDefaultCancellationToken cancellationToken
//                )
//                .ContinueWith(fun (t: Task<FormatSelectionResponse>) -> t.Result.AsFormatResponse())

        member _.ConfigurationAsync(filePath, ?cancellationToken: CancellationToken) : Task<FantomasResponse> =
            failwith "todo"
//            client.InvokeWithCancellationAsync<ConfigurationResponse>(
//                Methods.Configuration,
//                cancellationToken = orDefaultCancellationToken cancellationToken
//            )
