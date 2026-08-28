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

[<NoComparison; NoEquality>]
type ServiceState<'daemon> =
    {
        Daemons: Map<FantomasVersion, 'daemon>
        FolderToVersion: Map<Folder, FantomasVersion>
    }

    static member Empty: ServiceState<'daemon> =
        {
            Daemons = Map.empty
            FolderToVersion = Map.empty
        }

[<RequireQualifiedAccess>]
type GetDaemonError =
    | DotNetToolListError of error: DotNetToolListError
    | FantomasProcessStart of error: ProcessStartError
    | InCompatibleVersionFound
    | UnexpectedException of error: string

[<NoComparison>]
type Msg =
    | GetDaemon of folder: Folder * replyChannel: AsyncReplyChannel<Result<JsonRpc, GetDaemonError>>
    | Reset of AsyncReplyChannel<unit>

type IDaemon =
    inherit IDisposable
    abstract StartInfo: FantomasToolStartInfo
    abstract IsRunning: bool

[<NoComparison; NoEquality>]
type DaemonOperations<'daemon when 'daemon :> IDaemon> =
    {
        FindTool: Folder -> Result<FantomasToolFound, FantomasToolError>
        Create: FantomasVersion -> FantomasToolStartInfo -> Result<'daemon, ProcessStartError>
    }

/// Forget a version, and with it every folder that resolved to it. Leaving those folders behind
/// would pin them to a version with no daemon, which is the one state `resolveDaemon` cannot get
/// out of: it answers `CompatibleVersionIsKnownButNoDaemonIsRunning` and changes nothing, so the
/// next request answers the same way, for the rest of the session. Dropping them lets the next
/// request resolve the tool from scratch.
let forgetVersion (version: FantomasVersion) (state: ServiceState<'daemon>) : ServiceState<'daemon> =
    {
        Daemons = Map.remove version state.Daemons
        FolderToVersion = state.FolderToVersion |> Map.filter (fun _ known -> known <> version)
    }

let startDaemon
    (operations: DaemonOperations<'daemon>)
    (version: FantomasVersion)
    (startInfo: FantomasToolStartInfo)
    (folder: Folder)
    (state: ServiceState<'daemon>)
    : Result<'daemon, GetDaemonError> * ServiceState<'daemon>
    =
    match operations.Create version startInfo with
    | Error error -> Error(GetDaemonError.FantomasProcessStart error), forgetVersion version state
    | Ok daemon ->
        Ok daemon,
        {
            Daemons = Map.add version daemon state.Daemons
            FolderToVersion = Map.add folder version state.FolderToVersion
        }

let rec resolveDaemon
    (operations: DaemonOperations<'daemon>)
    (state: ServiceState<'daemon>)
    (folder: Folder)
    : Result<'daemon, GetDaemonError> * ServiceState<'daemon>
    =
    match Map.tryFind folder state.FolderToVersion with
    | Some version ->
        match Map.tryFind version state.Daemons with
        | Some daemon when (daemon :> IDaemon).IsRunning ->
            Ok daemon,
            { state with
                FolderToVersion = Map.add folder version state.FolderToVersion
            }
        | Some daemon ->
            // A weird situation where the process has crashed. Dispose what is left of it so the
            // handles it still holds are released, then reboot it the way it was started.
            let startInfo = (daemon :> IDaemon).StartInfo
            (daemon :> IDaemon).Dispose()
            startDaemon operations version startInfo folder state
        | None ->
            // A folder pinned to a version with no daemon behind it. Nothing here produces that
            // any more, because every path that drops a version forgets the folders pinned to it,
            // but it used to be answered with an error that changed nothing, so the folder gave
            // the same answer for the rest of the session. Forget the version and resolve the tool
            // from scratch: a cache that has lost track of a daemon should cost a lookup, not the
            // ability to format.
            resolveDaemon operations (forgetVersion version state) folder
    | None ->
        match operations.FindTool folder with
        | Ok(FantomasToolFound(version, startInfo)) ->
            // Daemons are keyed by version, not by folder. Two folders that pin the same Fantomas
            // share one, so a running daemon for this version has to be reused: starting a second
            // would overwrite the first in the map and leave its process running with nothing left
            // to dispose it.
            //
            // The reused daemon keeps the StartInfo it was created with, so a restart after a crash
            // resolves the tool the way the first folder did rather than the way this one just did.
            match Map.tryFind version state.Daemons with
            | Some daemon when (daemon :> IDaemon).IsRunning ->
                Ok daemon,
                { state with
                    FolderToVersion = Map.add folder version state.FolderToVersion
                }
            | running ->
                // A daemon that crashed is replaced, but dispose it first so the handles it still
                // holds are released. The replacement is started the way the dead one was, not the
                // way this folder resolves now, so that a daemon two folders share does not change
                // working directory depending on which of them happened to notice it had died.
                let startInfo =
                    match running with
                    | None -> startInfo
                    | Some daemon ->

                    let asItWasStarted = (daemon :> IDaemon).StartInfo
                    (daemon :> IDaemon).Dispose()
                    asItWasStarted

                startDaemon operations version startInfo folder state
        | Error FantomasToolError.NoCompatibleVersionFound -> Error GetDaemonError.InCompatibleVersionFound, state
        | Error(FantomasToolError.DotNetListError error) -> Error(GetDaemonError.DotNetToolListError error), state

/// A `RunningFantomasTool` as the cache sees it. A wrapper rather than an implementation on the
/// type itself, so that `IDaemon` stays inside this module: `RunningFantomasTool` is part of the
/// package, and it has no business growing an interface that exists to describe a cache.
type CachedDaemon(tool: RunningFantomasTool) =
    member _.Tool: RunningFantomasTool = tool

    interface IDaemon with
        member _.StartInfo = tool.StartInfo

        member _.IsRunning =
            // Both halves matter. A process that is up but whose connection has ended can never
            // answer again, and handing its `RpcClient` out means every request from then on fails
            // against a daemon the cache still believes in.
            not tool.Process.HasExited && not tool.RpcClient.Completion.IsCompleted

        member _.Dispose() = (tool :> IDisposable).Dispose()

/// Where a daemon was started from, for a log line to name.
let describeStartInfo (startInfo: FantomasToolStartInfo) : string =
    match startInfo with
    | FantomasToolStartInfo.LocalTool(Folder workingDirectory) -> $"a local tool in \"%s{workingDirectory}\""
    | FantomasToolStartInfo.GlobalTool -> "a global tool"
    | FantomasToolStartInfo.ToolOnPath(FantomasExecutableFile executableFile) -> $"the PATH, at \"%s{executableFile}\""

let createAgent
    (ct: CancellationToken)
    (onConfigurationWarning: ConfigurationWarning -> unit)
    (log: FantomasLogLevel -> string -> unit)
    : MailboxProcessor<Msg>
    =
    let operations: DaemonOperations<CachedDaemon> =
        {
            // Wrapped rather than passed straight through, because how a folder resolved is the
            // question a user asks when the wrong Fantomas formatted their code, and nothing in a
            // `FantomasResponse` answers it: a tool found on the PATH formats exactly as
            // successfully as the one their repository pins.
            FindTool =
                fun folder ->
                    let result: Result<FantomasToolFound, FantomasToolError> = findFantomasTool folder
                    let (Folder path) = folder

                    match result with
                    | Ok(FantomasToolFound(version, startInfo)) ->
                        log
                            FantomasLogLevel.Info
                            $"Resolved Fantomas %O{version} for \"%s{path}\" from %s{describeStartInfo startInfo}."
                    | Error FantomasToolError.NoCompatibleVersionFound ->
                        log
                            FantomasLogLevel.Warning
                            $"No compatible Fantomas for \"%s{path}\": no local tool, no global tool, and nothing on the PATH."
                    | Error(FantomasToolError.DotNetListError error) ->
                        log FantomasLogLevel.Error $"Could not list the dotnet tools for \"%s{path}\": %A{error}"

                    result
            Create =
                fun version startInfo ->
                    log
                        FantomasLogLevel.Info
                        $"Starting a Fantomas %O{version} daemon from %s{describeStartInfo startInfo}."

                    let result: Result<RunningFantomasTool, ProcessStartError> =
                        createForVersion version startInfo

                    match result with
                    | Ok _ -> ()
                    | Error error ->
                        log FantomasLogLevel.Error $"Could not start the Fantomas %O{version} daemon: %A{error}"

                    result
                    |> Result.map (fun daemon ->
                        // Subscribed here rather than where the daemon is handed out, so that it happens
                        // once per daemon however many folders end up sharing it.
                        daemon.ConfigurationWarnings.Add onConfigurationWarning
                        new CachedDaemon(daemon)
                    )
        }

    MailboxProcessor.Start(
        (fun inbox ->
            let rec messageLoop (state: ServiceState<CachedDaemon>) =
                async {
                    let! msg = inbox.Receive()

                    let nextState =
                        try
                            match msg with
                            | GetDaemon(folder, replyChannel) ->
                                let daemon, nextState = resolveDaemon operations state folder

                                replyChannel.Reply(
                                    Result.map (fun (daemon: CachedDaemon) -> daemon.Tool.RpcClient) daemon
                                )

                                nextState
                            | Reset replyChannel ->
                                Map.toList state.Daemons
                                |> List.iter (fun (_, daemon) -> (daemon :> IDaemon).Dispose())

                                replyChannel.Reply()
                                ServiceState.Empty
                        with ex ->
                            // This loop must not die. Every caller reaches it through a
                            // `PostAndReply` with no timeout, so an exception escaping here is not
                            // an error anyone sees: it ends the loop, and every request from then
                            // on waits forever on a reply that can never arrive. Resolving a tool
                            // shells out to `dotnet` and reads its pipes, which is where something
                            // unforeseen would come from.
                            //
                            // Answer the caller instead, and keep the daemons already running: the
                            // state is only advanced by an arm that got far enough to return one.
                            log
                                FantomasLogLevel.Error
                                $"The Fantomas daemon cache hit an unexpected error: %s{ex.Message}"

                            try
                                // Both arms throw before they reply, so this is the first answer
                                // the caller gets rather than a second one.
                                match msg with
                                | Reset replyChannel -> replyChannel.Reply()
                                | GetDaemon(_, replyChannel) ->
                                    replyChannel.Reply(Error(GetDaemonError.UnexpectedException ex.Message))
                            with _ ->
                                ()

                            state

                    return! messageLoop nextState
                }

            messageLoop ServiceState.Empty
        ),
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

let isCancellationRequested (requested: bool) : Result<unit, FantomasServiceError> =
    if requested then
        Error FantomasServiceError.CancellationWasRequested
    else
        Ok()

let getFolderFor (filePath: string) () : Result<Folder, FantomasServiceError> =
    if not (isPathAbsolute filePath) then
        Error FantomasServiceError.FilePathIsNotAbsolute
    elif not (File.Exists filePath) then
        Error FantomasServiceError.FileDoesNotExist
    else
        Path.GetDirectoryName filePath |> Folder |> Ok

let getDaemon (agent: MailboxProcessor<Msg>) (folder: Folder) : Result<JsonRpc, FantomasServiceError> =
    let daemon = agent.PostAndReply(fun replyChannel -> GetDaemon(folder, replyChannel))

    match daemon with
    | Ok daemon -> Ok daemon
    | Error gde -> Error(FantomasServiceError.DaemonNotFound gde)

let fileNotFoundResponse filePath : Task<FantomasResponse> =
    {
        Code = int FantomasResponseCode.FileNotFound
        FilePath = filePath
        Content = Some $"File \"%s{filePath}\" does not exist."
        SelectedRange = None
        Cursor = None
    }
    |> Task.FromResult

let fileNotAbsoluteResponse filePath : Task<FantomasResponse> =
    {
        Code = int FantomasResponseCode.FilePathIsNotAbsolute
        FilePath = filePath
        Content = Some $"\"%s{filePath}\" is not an absolute file path. Relative paths are not supported."
        SelectedRange = None
        Cursor = None
    }
    |> Task.FromResult

let daemonNotFoundResponse filePath (error: GetDaemonError) : Task<FantomasResponse> =
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
            $"Fantomas.Client tried to run `%s{executableFile} %s{arguments}` inside working directory \"%s{workingDirectory}\" but could not find \"%s{executableFile}\" on the PATH (%s{pathEnvironmentVariable}). Error: %s{error}",
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
            $"Fantomas.Client tried to run `%s{executableFile} %s{arguments}` but exited with code %i{exitCode} %s{error}",
            FantomasResponseCode.DaemonCreationFailed
        | GetDaemonError.InCompatibleVersionFound ->
            "Fantomas.Client did not found a compatible dotnet tool version to launch as daemon process",
            FantomasResponseCode.ToolNotFound
        | GetDaemonError.UnexpectedException error ->
            $"Fantomas.Client hit an unexpected error while resolving a daemon: %s{error}",
            FantomasResponseCode.DaemonCreationFailed

    {
        Code = int code
        FilePath = filePath
        Content = Some content
        SelectedRange = None
        Cursor = None
    }
    |> Task.FromResult

let cancellationWasRequestedResponse filePath : Task<FantomasResponse> =
    {
        Code = int FantomasResponseCode.CancellationWasRequested
        FilePath = filePath
        Content = Some "FantomasService is being or has been disposed."
        SelectedRange = None
        Cursor = None
    }
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
        {
            Code = int FantomasResponseCode.Error
            FilePath = inputFilePath
            Content = Some msg
            SelectedRange = None
            Cursor = None
        }

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

            {
                Code = int FantomasResponseCode.Formatted
                FilePath = fileName
                Content = Some formattedContent
                SelectedRange = None
                Cursor = None
            }
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

                Some(FormatCursorPosition(cursorObject.["Line"].Value<int>(), cursorObject.["Column"].Value<int>()))

            {
                Code = int FantomasResponseCode.Formatted
                FilePath = fileName
                Content = Some formattedContent
                SelectedRange = None
                Cursor = cursor
            }

        | "Unchanged" when fields.Count = 1 ->
            let fileName = fields.[0].Value<string>()

            {
                Code = int FantomasResponseCode.UnChanged
                FilePath = fileName
                Content = None
                SelectedRange = None
                Cursor = None
            }
        | "Error" when fields.Count = 2 ->
            let fileName = fields.[0].Value<string>()
            let formattingError = fields.[1].Value<string>()

            {
                Code = int FantomasResponseCode.Error
                FilePath = fileName
                Content = Some formattingError
                SelectedRange = None
                Cursor = None
            }
        | "IgnoredFile" when fields.Count = 1 ->
            let fileName = fields.[0].Value<string>()

            {
                Code = int FantomasResponseCode.Ignored
                FilePath = fileName
                Content = None
                SelectedRange = None
                Cursor = None
            }
        | _ ->
            mkError
                $"Could not deserialize the message from the daemon, got unexpected case name %s{caseName} with %i{fields.Count} fields."

    with ex ->
        mkError $"Could not deserialize the message from the daemon, %s{ex.Message}"

type LSPFantomasService(log: Action<FantomasLogLevel, string>) =
    let cts = new CancellationTokenSource()
    let configurationWarnings = Event<ConfigurationWarning>()

    let agent: MailboxProcessor<Msg> =
        createAgent
            cts.Token
            configurationWarnings.Trigger
            (fun level message ->
                try
                    log.Invoke(level, message)
                with _ ->
                    // A log delegate that throws must not take the service down with it. These are
                    // called from inside the mailbox that resolves daemons, and an exception out of
                    // there ends the loop: `getDaemon` then waits on a reply that can never come,
                    // so every format request from then on hangs rather than fails. Losing a log
                    // line is the cheaper end of that trade.
                    ()
            )

    new() = new LSPFantomasService(Action<FantomasLogLevel, string>(fun _ _ -> ()))

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
                        {
                            Code = int FantomasResponseCode.Version
                            Content = Some t.Result
                            FilePath = filePath
                            SelectedRange = None
                            Cursor = None
                        }
                    )
            )
            |> mapResultToResponse filePath

        member _.FormatDocumentAsync
            (formatDocumentOptions: FormatDocumentRequest, ?cancellationToken: CancellationToken)
            : Task<FantomasResponse>
            =
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
                    .ContinueWith(fun (t: Task<JObject>) -> decodeFormatResult formatDocumentOptions.FilePath t.Result)
            )
            |> mapResultToResponse formatDocumentOptions.FilePath

        member _.FormatSelectionAsync
            (formatSelectionRequest: FormatSelectionRequest, ?cancellationToken: CancellationToken)
            =
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
                    .ContinueWith(fun (t: Task<FormatSelectionResponse>) -> t.Result.AsFormatResponse())
            )
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

                        {
                            Code = int FantomasResponseCode.Configuration
                            FilePath = filePath
                            Content = Some t.Result
                            SelectedRange = None
                            Cursor = None
                        }
                    )
            )
            |> mapResultToResponse filePath

        member _.ConfigurationWarnings = configurationWarnings.Publish

        member _.ClearCache() = agent.PostAndReply Reset
