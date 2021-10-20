module Fantomas.Client.LSPFantomasService

open System
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
    | Reset

let private createAgent (ct: CancellationToken) =
    MailboxProcessor.Start(
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


                        | Reset ->
                            Map.toList state.Daemons
                            |> List.iter (fun (_, daemon) -> daemon.Dispose())

                            ServiceState.Empty

                    return! messageLoop nextState
                }

            messageLoop ServiceState.Empty),
        cancellationToken = ct
    )

type FantomasServiceError =
    | DaemonNotFound
    | FileDoesNotExist
    | FilePathIsNotAbsolute

let isPathAbsolute (path: string) : bool =
    if
        String.IsNullOrWhiteSpace path
        || path.IndexOfAny(Path.GetInvalidPathChars()) <> -1
        || not (Path.IsPathRooted path)
    then
        false
    else
        let pathRoot = Path.GetPathRoot path
        // Accepts X:\ and \\UNC\PATH, rejects empty string, \ and X:, but accepts / to support Linux
        if pathRoot.Length <= 2 && pathRoot <> "/" then
            false
        else if pathRoot.[0] <> '\\' || pathRoot.[1] <> '\\' then
            true
        else
            pathRoot.Trim('\\').IndexOf('\\') <> -1 // A UNC server name without a share name (e.g "\\NAME" or "\\NAME\") is invalid

let private getFolderFor (filePath: string) : Result<Folder, FantomasServiceError> =
    if not (isPathAbsolute filePath) then
        Error FantomasServiceError.FilePathIsNotAbsolute
    elif not (File.Exists filePath) then
        Error FantomasServiceError.FileDoesNotExist
    else
        Path.GetDirectoryName filePath |> Folder |> Ok

let private getDaemon (agent: MailboxProcessor<Msg>) (folder: Folder) : Result<JsonRpc, FantomasServiceError> =
    let daemon =
        agent.PostAndReply(fun replyChannel -> GetDaemon(folder, replyChannel))

    match daemon with
    | Some daemon -> Ok daemon
    | None -> Error FantomasServiceError.DaemonNotFound

let private fileNotFoundResponse filePath : Task<FantomasResponse> =
    { Code = int FantomasResponseCode.FileNotFound
      FilePath = filePath
      Content = Some(sprintf "File \"%s\" does not exist" filePath) }
    |> Task.FromResult

let private fileNotAbsoluteResponse filePath : Task<FantomasResponse> =
    { Code = int FantomasResponseCode.FilePathIsNotAbsolute
      FilePath = filePath
      Content = Some(sprintf "\"%s\" is not an absolute file path. Relative paths are not supported" filePath) }
    |> Task.FromResult

let private daemonNotFoundResponse filePath : Task<FantomasResponse> =
    { Code = int FantomasResponseCode.ToolNotFound
      FilePath = filePath
      Content = Some(sprintf "No fantomas tool was found for \"%s\"" filePath) }
    |> Task.FromResult

let mapResultToResponse (filePath: string) (result: Result<Task<FantomasResponse>, FantomasServiceError>) =
    match result with
    | Ok t -> t
    | Error FantomasServiceError.FileDoesNotExist -> fileNotFoundResponse filePath
    | Error FantomasServiceError.FilePathIsNotAbsolute -> fileNotAbsoluteResponse filePath
    | Error FantomasServiceError.DaemonNotFound -> daemonNotFoundResponse filePath

type LSPFantomasService() =
    let cts = new CancellationTokenSource()
    let agent = createAgent cts.Token

    interface FantomasService with
        member this.Dispose() =
            agent.Post Reset
            cts.Cancel()

        member _.VersionAsync(filePath, ?cancellationToken: CancellationToken) : Task<FantomasResponse> =
            getFolderFor filePath
            |> Result.bind (getDaemon agent)
            |> Result.map
                (fun client ->
                    client
                        .InvokeWithCancellationAsync<string>(
                            Methods.Version,
                            cancellationToken = orDefaultCancellationToken cancellationToken
                        )
                        .ContinueWith(fun (t: Task<string>) ->
                            { Code = int FantomasResponseCode.Version
                              Content = Some t.Result
                              FilePath = filePath }))
            |> mapResultToResponse filePath

        member _.FormatDocumentAsync
            (
                formatDocumentOptions: FormatDocumentRequest,
                ?cancellationToken: CancellationToken
            ) : Task<FantomasResponse> =
            getFolderFor formatDocumentOptions.FilePath
            |> Result.bind (getDaemon agent)
            |> Result.map
                (fun client ->
                    client
                        .InvokeWithParameterObjectAsync<FormatDocumentResponse>(
                            Methods.FormatDocument,
                            argument = formatDocumentOptions,
                            cancellationToken = orDefaultCancellationToken cancellationToken
                        )
                        .ContinueWith(fun (t: Task<FormatDocumentResponse>) -> t.Result.AsFormatResponse()))
            |> mapResultToResponse formatDocumentOptions.FilePath

        member _.FormatSelectionAsync
            (
                formatSelectionRequest: FormatSelectionRequest,
                ?cancellationToken: CancellationToken
            ) =
            getFolderFor formatSelectionRequest.FilePath
            |> Result.bind (getDaemon agent)
            |> Result.map
                (fun client ->
                    client
                        .InvokeWithParameterObjectAsync<FormatSelectionResponse>(
                            Methods.FormatSelection,
                            argument = formatSelectionRequest,
                            cancellationToken = orDefaultCancellationToken cancellationToken
                        )
                        .ContinueWith(fun (t: Task<FormatSelectionResponse>) -> t.Result.AsFormatResponse()))
            |> mapResultToResponse formatSelectionRequest.FilePath

        member _.ConfigurationAsync(filePath, ?cancellationToken: CancellationToken) : Task<FantomasResponse> =
            getFolderFor filePath
            |> Result.bind (getDaemon agent)
            |> Result.map
                (fun client ->
                    client
                        .InvokeWithCancellationAsync<string>(
                            Methods.Configuration,
                            cancellationToken = orDefaultCancellationToken cancellationToken
                        )
                        .ContinueWith(fun (t: Task<string>) ->

                            { Code = int FantomasResponseCode.Configuration
                              FilePath = filePath
                              Content = Some t.Result }))
            |> mapResultToResponse filePath

        member _.ClearCache() = agent.Post Reset
