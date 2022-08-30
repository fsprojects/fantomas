module Fantomas.Client.LSPFantomasService

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open StreamJsonRpc
open Fantomas.Client.Contracts
open Fantomas.Client.LSPFantomasServiceTypes
open Fantomas.Client.FantomasToolLocator

type ServiceState =
    { Daemons: Map<FantomasVersion, RunningFantomasTool>
      FolderToVersion: Map<Folder, FantomasVersion> }

    static member Empty: ServiceState =
        { Daemons = Map.empty
          FolderToVersion = Map.empty }

[<RequireQualifiedAccess>]
type GetDaemonError =
    | DotNetToolListError of error: DotNetToolListError
    | FantomasProcessStart of error: ProcessStartError
    | InCompatibleVersionFound
    | CompatibleVersionIsKnownButNoDaemonIsRunning of version: FantomasVersion

type Msg =
    | GetDaemon of Folder * AsyncReplyChannel<Result<JsonRpc, GetDaemonError>>
    | Reset of AsyncReplyChannel<unit>

let private createAgent (ct: CancellationToken) =
    MailboxProcessor.Start(
        (fun inbox ->
            let rec messageLoop (state: ServiceState) =
                async {
                    let! msg = inbox.Receive()

                    let nextState =
                        match msg with
                        | GetDaemon (folder, replyChannel) ->
                            // get the version for that folder
                            // look in the cache first
                            let versionFromCache = Map.tryFind folder state.FolderToVersion

                            match versionFromCache with
                            | Some version ->
                                let daemon = Map.tryFind version state.Daemons

                                match daemon with
                                | Some daemon ->
                                    // We have a daemon for the required version in the cache, check if we can still use it.
                                    if daemon.Process.HasExited then
                                        // weird situation where the process has crashed.
                                        // Trying to reboot
                                        (daemon :> IDisposable).Dispose()

                                        let newDaemonResult = createFor daemon.StartInfo

                                        match newDaemonResult with
                                        | Ok newDaemon ->
                                            replyChannel.Reply(Ok newDaemon.RpcClient)

                                            { state with
                                                FolderToVersion = Map.add folder version state.FolderToVersion
                                                Daemons = Map.add version newDaemon state.Daemons }
                                        | Error pse ->
                                            replyChannel.Reply(Error(GetDaemonError.FantomasProcessStart pse))
                                            state
                                    else
                                        // return running client
                                        replyChannel.Reply(Ok daemon.RpcClient)

                                        { state with FolderToVersion = Map.add folder version state.FolderToVersion }
                                | None ->
                                    // This is a strange situation, we know what version is linked to that folder but there is no daemon
                                    // The moment a version is added, is also the moment a daemon is re-used or created
                                    replyChannel.Reply(
                                        Error(GetDaemonError.CompatibleVersionIsKnownButNoDaemonIsRunning version)
                                    )

                                    state
                            | None ->
                                // Try and find a version of fantomas daemon for our current folder
                                let fantomasToolResult: Result<FantomasToolFound, FantomasToolError> =
                                    findFantomasTool folder

                                match fantomasToolResult with
                                | Ok (FantomasToolFound (version, startInfo)) ->
                                    let createDaemonResult = createFor startInfo

                                    match createDaemonResult with
                                    | Ok daemon ->
                                        replyChannel.Reply(Ok daemon.RpcClient)

                                        { state with
                                            Daemons = Map.add version daemon state.Daemons
                                            FolderToVersion = Map.add folder version state.FolderToVersion }
                                    | Error pse ->
                                        replyChannel.Reply(Error(GetDaemonError.FantomasProcessStart pse))
                                        state
                                | Error FantomasToolError.NoCompatibleVersionFound ->
                                    replyChannel.Reply(Error GetDaemonError.InCompatibleVersionFound)
                                    state
                                | Error (FantomasToolError.DotNetListError dotNetToolListError) ->
                                    replyChannel.Reply(Error(GetDaemonError.DotNetToolListError dotNetToolListError))
                                    state
                        | Reset replyChannel ->
                            Map.toList state.Daemons
                            |> List.iter (fun (_, daemon) -> (daemon :> IDisposable).Dispose())

                            replyChannel.Reply()
                            ServiceState.Empty

                    return! messageLoop nextState
                }

            messageLoop ServiceState.Empty),
        cancellationToken = ct
    )

type FantomasServiceError =
    | DaemonNotFound of GetDaemonError
    | FileDoesNotExist
    | FilePathIsNotAbsolute
    | CancellationWasRequested

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

let private isCancellationRequested (requested: bool) : Result<unit, FantomasServiceError> =
    if requested then
        Error FantomasServiceError.CancellationWasRequested
    else
        Ok()

let private getFolderFor (filePath: string) () : Result<Folder, FantomasServiceError> =
    if not (isPathAbsolute filePath) then
        Error FantomasServiceError.FilePathIsNotAbsolute
    elif not (File.Exists filePath) then
        Error FantomasServiceError.FileDoesNotExist
    else
        Path.GetDirectoryName filePath |> Folder |> Ok

let private getDaemon (agent: MailboxProcessor<Msg>) (folder: Folder) : Result<JsonRpc, FantomasServiceError> =
    let daemon = agent.PostAndReply(fun replyChannel -> GetDaemon(folder, replyChannel))

    match daemon with
    | Ok daemon -> Ok daemon
    | Error gde -> Error(FantomasServiceError.DaemonNotFound gde)

let private fileNotFoundResponse filePath : Task<FantomasResponse> =
    { Code = int FantomasResponseCode.FileNotFound
      FilePath = filePath
      Content = Some $"File \"%s{filePath}\" does not exist."
      SelectedRange = None }
    |> Task.FromResult

let private fileNotAbsoluteResponse filePath : Task<FantomasResponse> =
    { Code = int FantomasResponseCode.FilePathIsNotAbsolute
      FilePath = filePath
      Content = Some $"\"%s{filePath}\" is not an absolute file path. Relative paths are not supported."
      SelectedRange = None }
    |> Task.FromResult

let private daemonNotFoundResponse filePath (error: GetDaemonError) : Task<FantomasResponse> =
    let content, code =
        match error with
        | GetDaemonError.DotNetToolListError (DotNetToolListError.ProcessStartError (ProcessStartError.ExecutableFileNotFound (executableFile,
                                                                                                                               arguments,
                                                                                                                               workingDirectory,
                                                                                                                               pathEnvironmentVariable,
                                                                                                                               error)))
        | GetDaemonError.FantomasProcessStart (ProcessStartError.ExecutableFileNotFound (executableFile,
                                                                                         arguments,
                                                                                         workingDirectory,
                                                                                         pathEnvironmentVariable,
                                                                                         error)) ->
            $"Fantomas.Client tried to run `%s{executableFile} %s{arguments}` inside working directory \"{workingDirectory}\" but could not find \"%s{executableFile}\" on the PATH (%s{pathEnvironmentVariable}). Error: %s{error}",
            FantomasResponseCode.DaemonCreationFailed
        | GetDaemonError.DotNetToolListError (DotNetToolListError.ProcessStartError (ProcessStartError.UnExpectedException (executableFile,
                                                                                                                            arguments,
                                                                                                                            error)))
        | GetDaemonError.FantomasProcessStart (ProcessStartError.UnExpectedException (executableFile, arguments, error)) ->
            $"Fantomas.Client tried to run `%s{executableFile} %s{arguments}` but failed with \"%s{error}\"",
            FantomasResponseCode.DaemonCreationFailed
        | GetDaemonError.DotNetToolListError (DotNetToolListError.ExitCodeNonZero (executableFile,
                                                                                   arguments,
                                                                                   exitCode,
                                                                                   error)) ->
            $"Fantomas.Client tried to run `%s{executableFile} %s{arguments}` but exited with code {exitCode} {error}",
            FantomasResponseCode.DaemonCreationFailed
        | GetDaemonError.InCompatibleVersionFound ->
            "Fantomas.Client did not found a compatible dotnet tool version to launch as daemon process",
            FantomasResponseCode.ToolNotFound
        | GetDaemonError.CompatibleVersionIsKnownButNoDaemonIsRunning (FantomasVersion version) ->
            $"Fantomas.Client found a compatible version `%s{version}` but no daemon could be launched.",
            FantomasResponseCode.DaemonCreationFailed

    { Code = int code
      FilePath = filePath
      Content = Some content
      SelectedRange = None }
    |> Task.FromResult

let private cancellationWasRequestedResponse filePath : Task<FantomasResponse> =
    { Code = int FantomasResponseCode.CancellationWasRequested
      FilePath = filePath
      Content = Some "FantomasService is being or has been disposed."
      SelectedRange = None }
    |> Task.FromResult

let mapResultToResponse (filePath: string) (result: Result<Task<FantomasResponse>, FantomasServiceError>) =
    match result with
    | Ok t -> t
    | Error FantomasServiceError.FileDoesNotExist -> fileNotFoundResponse filePath
    | Error FantomasServiceError.FilePathIsNotAbsolute -> fileNotAbsoluteResponse filePath
    | Error (FantomasServiceError.DaemonNotFound e) -> daemonNotFoundResponse filePath e
    | Error FantomasServiceError.CancellationWasRequested -> cancellationWasRequestedResponse filePath

type LSPFantomasService() =
    let cts = new CancellationTokenSource()
    let agent = createAgent cts.Token

    interface FantomasService with
        member this.Dispose() =
            if not cts.IsCancellationRequested then
                agent.PostAndReply Reset
                cts.Cancel()

        member _.VersionAsync(filePath, ?cancellationToken: CancellationToken) : Task<FantomasResponse> =
            isCancellationRequested cts.IsCancellationRequested
            |> Result.bind (getFolderFor filePath)
            |> Result.bind (getDaemon agent)
            |> Result.map (fun client ->
                client
                    .InvokeWithCancellationAsync<string>(
                        Methods.Version,
                        cancellationToken = Option.defaultValue cts.Token cancellationToken
                    )
                    .ContinueWith(fun (t: Task<string>) ->
                        { Code = int FantomasResponseCode.Version
                          Content = Some t.Result
                          FilePath = filePath
                          SelectedRange = None }))
            |> mapResultToResponse filePath

        member _.FormatDocumentAsync
            (
                formatDocumentOptions: FormatDocumentRequest,
                ?cancellationToken: CancellationToken
            ) : Task<FantomasResponse> =
            isCancellationRequested cts.IsCancellationRequested
            |> Result.bind (getFolderFor formatDocumentOptions.FilePath)
            |> Result.bind (getDaemon agent)
            |> Result.map (fun client ->
                client
                    .InvokeWithParameterObjectAsync<FormatDocumentResponse>(
                        Methods.FormatDocument,
                        argument = formatDocumentOptions,
                        cancellationToken = Option.defaultValue cts.Token cancellationToken
                    )
                    .ContinueWith(fun (t: Task<FormatDocumentResponse>) -> t.Result.AsFormatResponse()))
            |> mapResultToResponse formatDocumentOptions.FilePath

        member _.FormatSelectionAsync
            (
                formatSelectionRequest: FormatSelectionRequest,
                ?cancellationToken: CancellationToken
            ) =
            isCancellationRequested cts.IsCancellationRequested
            |> Result.bind (getFolderFor formatSelectionRequest.FilePath)
            |> Result.bind (getDaemon agent)
            |> Result.map (fun client ->
                client
                    .InvokeWithParameterObjectAsync<FormatSelectionResponse>(
                        Methods.FormatSelection,
                        argument = formatSelectionRequest,
                        cancellationToken = Option.defaultValue cts.Token cancellationToken
                    )
                    .ContinueWith(fun (t: Task<FormatSelectionResponse>) -> t.Result.AsFormatResponse()))
            |> mapResultToResponse formatSelectionRequest.FilePath

        member _.ConfigurationAsync(filePath, ?cancellationToken: CancellationToken) : Task<FantomasResponse> =
            isCancellationRequested cts.IsCancellationRequested
            |> Result.bind (getFolderFor filePath)
            |> Result.bind (getDaemon agent)
            |> Result.map (fun client ->
                client
                    .InvokeWithCancellationAsync<string>(
                        Methods.Configuration,
                        cancellationToken = Option.defaultValue cts.Token cancellationToken
                    )
                    .ContinueWith(fun (t: Task<string>) ->

                        { Code = int FantomasResponseCode.Configuration
                          FilePath = filePath
                          Content = Some t.Result
                          SelectedRange = None }))
            |> mapResultToResponse filePath

        member _.ClearCache() = agent.PostAndReply Reset
