module Fantomas.Client.LSPFantomasService

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open Newtonsoft.Json.Linq
open StreamJsonRpc
open Fantomas.Client.Contracts
open Fantomas.Client.LSPFantomasServiceTypes
open Fantomas.Client.FantomasToolLocator

[<NoComparison>]
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

[<NoComparison>]
type Msg =
    | GetDaemon of folder: Folder * replyChannel: AsyncReplyChannel<Result<JsonRpc, GetDaemonError>>
    | Reset of AsyncReplyChannel<unit>

let private createAgent (ct: CancellationToken) =
    MailboxProcessor.Start(
        (fun inbox ->
            let rec messageLoop (state: ServiceState) =
                async {
                    let! msg = inbox.Receive()

                    let nextState =
                        match msg with
                        | GetDaemon(folder, replyChannel) ->
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

                                            { FolderToVersion = Map.add folder version state.FolderToVersion
                                              Daemons = Map.add version newDaemon state.Daemons }
                                        | Error pse ->
                                            replyChannel.Reply(Error(GetDaemonError.FantomasProcessStart pse))
                                            state
                                    else
                                        // return running client
                                        replyChannel.Reply(Ok daemon.RpcClient)

                                        { state with
                                            FolderToVersion = Map.add folder version state.FolderToVersion }
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
                                | Ok(FantomasToolFound(version, startInfo)) ->
                                    let createDaemonResult = createFor startInfo

                                    match createDaemonResult with
                                    | Ok daemon ->
                                        replyChannel.Reply(Ok daemon.RpcClient)

                                        { Daemons = Map.add version daemon state.Daemons
                                          FolderToVersion = Map.add folder version state.FolderToVersion }
                                    | Error pse ->
                                        replyChannel.Reply(Error(GetDaemonError.FantomasProcessStart pse))
                                        state
                                | Error FantomasToolError.NoCompatibleVersionFound ->
                                    replyChannel.Reply(Error GetDaemonError.InCompatibleVersionFound)
                                    state
                                | Error(FantomasToolError.DotNetListError dotNetToolListError) ->
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
      SelectedRange = None
      Cursor = None }
    |> Task.FromResult

let private fileNotAbsoluteResponse filePath : Task<FantomasResponse> =
    { Code = int FantomasResponseCode.FilePathIsNotAbsolute
      FilePath = filePath
      Content = Some $"\"%s{filePath}\" is not an absolute file path. Relative paths are not supported."
      SelectedRange = None
      Cursor = None }
    |> Task.FromResult

let private daemonNotFoundResponse filePath (error: GetDaemonError) : Task<FantomasResponse> =
    let content, code =
        match error with
        | GetDaemonError.DotNetToolListError(DotNetToolListError.ProcessStartError(ProcessStartError.ExecutableFileNotFound(executableFile,
                                                                                                                            arguments,
                                                                                                                            workingDirectory,
                                                                                                                            pathEnvironmentVariable,
                                                                                                                            error)))
        | GetDaemonError.FantomasProcessStart(ProcessStartError.ExecutableFileNotFound(executableFile,
                                                                                       arguments,
                                                                                       workingDirectory,
                                                                                       pathEnvironmentVariable,
                                                                                       error)) ->
            $"Fantomas.Client tried to run `%s{executableFile} %s{arguments}` inside working directory \"{workingDirectory}\" but could not find \"%s{executableFile}\" on the PATH (%s{pathEnvironmentVariable}). Error: %s{error}",
            FantomasResponseCode.DaemonCreationFailed
        | GetDaemonError.DotNetToolListError(DotNetToolListError.ProcessStartError(ProcessStartError.UnExpectedException(executableFile,
                                                                                                                         arguments,
                                                                                                                         error)))
        | GetDaemonError.FantomasProcessStart(ProcessStartError.UnExpectedException(executableFile, arguments, error)) ->
            $"Fantomas.Client tried to run `%s{executableFile} %s{arguments}` but failed with \"%s{error}\"",
            FantomasResponseCode.DaemonCreationFailed
        | GetDaemonError.DotNetToolListError(DotNetToolListError.ExitCodeNonZero(executableFile,
                                                                                 arguments,
                                                                                 exitCode,
                                                                                 error)) ->
            $"Fantomas.Client tried to run `%s{executableFile} %s{arguments}` but exited with code {exitCode} {error}",
            FantomasResponseCode.DaemonCreationFailed
        | GetDaemonError.InCompatibleVersionFound ->
            "Fantomas.Client did not found a compatible dotnet tool version to launch as daemon process",
            FantomasResponseCode.ToolNotFound
        | GetDaemonError.CompatibleVersionIsKnownButNoDaemonIsRunning(FantomasVersion version) ->
            $"Fantomas.Client found a compatible version `%s{version}` but no daemon could be launched.",
            FantomasResponseCode.DaemonCreationFailed

    { Code = int code
      FilePath = filePath
      Content = Some content
      SelectedRange = None
      Cursor = None }
    |> Task.FromResult

let private cancellationWasRequestedResponse filePath : Task<FantomasResponse> =
    { Code = int FantomasResponseCode.CancellationWasRequested
      FilePath = filePath
      Content = Some "FantomasService is being or has been disposed."
      SelectedRange = None
      Cursor = None }
    |> Task.FromResult

let mapResultToResponse (filePath: string) (result: Result<Task<FantomasResponse>, FantomasServiceError>) =
    match result with
    | Ok t -> t
    | Error FantomasServiceError.FileDoesNotExist -> fileNotFoundResponse filePath
    | Error FantomasServiceError.FilePathIsNotAbsolute -> fileNotAbsoluteResponse filePath
    | Error(FantomasServiceError.DaemonNotFound e) -> daemonNotFoundResponse filePath e
    | Error FantomasServiceError.CancellationWasRequested -> cancellationWasRequestedResponse filePath

/// <summary>
/// <para>
/// The Fantomas daemon currently sends a Fantomas.Client.LSPFantomasServiceTypes.FormatDocumentResponse back to Fantomas.Client.
/// This was a poor choice as the serialization of a DU case breaks when you add a new field to it. Even though that field is optional.
/// To overcome this, we deserialize the FormatDocumentResponse ourselves to construct the matching FantomasResponse.
/// </para>
/// <para>
/// In v6.0 we introduced an additional option field to FormatDocumentResponse.Formatted being the cursor position.
/// That is why we currently have two match cases that try to deserialize "Formatted".
/// </para>
/// </summary>
/// <param name="inputFilePath">When serialization fails, we re-use the input file path from the request information.</param>
/// <param name="json">The raw JObject that send sent over the wire.</param>
let decodeFormatResult (inputFilePath: string) (json: JObject) : FantomasResponse =
    let mkError msg =
        { Code = int FantomasResponseCode.Error
          FilePath = inputFilePath
          Content = Some msg
          SelectedRange = None
          Cursor = None }

    try
        if not (json.ContainsKey("Case")) || not (json.ContainsKey("Fields")) then
            mkError "Expected \"Case\" and \"Fields\" to be present in the response json"
        else
            let caseName = json.["Case"].Value<string>()
            let fields = json.["Fields"].Value<JArray>()

            match caseName with
            | "Formatted" when fields.Count = 2 ->
                let fileName = fields.[0].Value<string>()
                let formattedContent = fields.[1].Value<string>()

                { Code = int FantomasResponseCode.Formatted
                  FilePath = fileName
                  Content = Some formattedContent
                  SelectedRange = None
                  Cursor = None }
            | "Formatted" when fields.Count = 3 ->
                let fileName = fields.[0].Value<string>()
                let formattedContent = fields.[1].Value<string>()

                let cursor =
                    if fields.[2].Type = JTokenType.Null then
                        None
                    else
                        // This is wrapped as an option, the Case is "Some" here.
                        // We need to extract the Line and Column from the first item in Fields
                        let cursorObject = fields.[2].Value<JObject>()
                        let cursorObject = cursorObject.["Fields"].[0].Value<JObject>()

                        Some(
                            FormatCursorPosition(
                                cursorObject.["Line"].Value<int>(),
                                cursorObject.["Column"].Value<int>()
                            )
                        )

                { Code = int FantomasResponseCode.Formatted
                  FilePath = fileName
                  Content = Some formattedContent
                  SelectedRange = None
                  Cursor = cursor }

            | "Unchanged" when fields.Count = 1 ->
                let fileName = fields.[0].Value<string>()

                { Code = int FantomasResponseCode.UnChanged
                  FilePath = fileName
                  Content = None
                  SelectedRange = None
                  Cursor = None }
            | "Error" when fields.Count = 2 ->
                let fileName = fields.[0].Value<string>()
                let formattingError = fields.[1].Value<string>()

                { Code = int FantomasResponseCode.Error
                  FilePath = fileName
                  Content = Some formattingError
                  SelectedRange = None
                  Cursor = None }
            | "IgnoredFile" when fields.Count = 1 ->
                let fileName = fields.[0].Value<string>()

                { Code = int FantomasResponseCode.Ignored
                  FilePath = fileName
                  Content = None
                  SelectedRange = None
                  Cursor = None }
            | _ ->
                mkError
                    $"Could not deserialize the message from the daemon, got unexpected case name %s{caseName} with %i{fields.Count} fields."

    with ex ->
        mkError $"Could not deserialize the message from the daemon, %s{ex.Message}"

type LSPFantomasService() =
    let cts = new CancellationTokenSource()
    let agent = createAgent cts.Token

    interface FantomasService with
        member this.Dispose() =
            if not cts.IsCancellationRequested then
                let _ = agent.PostAndReply Reset
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
                          SelectedRange = None
                          Cursor = None }))
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
                    .InvokeWithParameterObjectAsync<JObject>(
                        Methods.FormatDocument,
                        argument = formatDocumentOptions,
                        cancellationToken = Option.defaultValue cts.Token cancellationToken
                    )
                    .ContinueWith(fun (t: Task<JObject>) -> decodeFormatResult formatDocumentOptions.FilePath t.Result))
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
                          SelectedRange = None
                          Cursor = None }))
            |> mapResultToResponse filePath

        member _.ClearCache() = agent.PostAndReply Reset
