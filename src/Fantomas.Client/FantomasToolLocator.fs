module Fantomas.Client.FantomasToolLocator

open System
open System.Collections.Generic
open System.ComponentModel
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open System.Runtime.InteropServices
open Newtonsoft.Json.Linq
open StreamJsonRpc
open Fantomas.Client.Contracts
open Fantomas.Client.LSPFantomasServiceTypes

// Only 4.6.0-alpha-004 has daemon capabilities
let supportedRange = SemanticVersioning.Range(">=v4.6.0-alpha-004")

[<return: Struct>]
let (|CompatibleVersion|_|) (version: string) : string voption =
    match SemanticVersioning.Version.TryParse version with
    | true, parsedVersion ->
        if supportedRange.IsSatisfied(parsedVersion, includePrerelease = true) then
            ValueSome version
        else
            ValueNone
    | _ -> ValueNone

// In the past, fantomas was named fantomas-tool.
[<return: Struct>]
let (|CompatibleToolName|_|) (toolName: string) : string voption =
    if toolName = "fantomas-tool" || toolName = "fantomas" then
        ValueSome toolName
    else
        ValueNone

let readOutputStreamAsLines (outputStream: StreamReader) : string list =
    let rec readLines (outputStream: StreamReader) (continuation: string list -> string list) =
        let nextLine = outputStream.ReadLine()

        if isNull nextLine then
            continuation []
        else
            readLines outputStream (fun lines -> nextLine :: lines |> continuation)

    readLines outputStream id

let startProcess (ps: ProcessStartInfo) : Result<Process, ProcessStartError> =
    try
        Ok(Process.Start ps)
    with
    | :? Win32Exception as win32ex ->
        let pathEnv = Environment.GetEnvironmentVariable "PATH"

        Error(
            ProcessStartError.ExecutableFileNotFound(
                ps.FileName,
                ps.Arguments,
                ps.WorkingDirectory,
                pathEnv,
                win32ex.Message
            )
        )
    | ex -> Error(ProcessStartError.UnExpectedException(ps.FileName, ps.Arguments, ex.Message))

/// One tool of what `dotnet tool list` reported, however that SDK chose to print it.
[<NoComparison>]
type ListedTool = { PackageId: string; Version: string }

let packageSidVersionRegex: Regex = Regex(@"^Package\sId\s+Version.+$")

/// The rows of the table `dotnet tool list` prints for a person to read. Empty for anything not
/// recognised as that table, which is the answer that keeps a mistake cheap: `findFantomasTool`
/// treats finding nothing as a reason to go on looking, where a row read out of the wrong column
/// would have it start a daemon on a version nobody asked for.
let toolsFromTable (lines: string list) : ListedTool list =
    // These are local bindings, which cannot carry an attribute, so they stay on option.
    let (|HeaderLine|_|) (line: string) : unit option =
        if packageSidVersionRegex.IsMatch line then Some() else None

    let (|Dashes|_|) (line: string) : unit option =
        if String.forall ((=) '-') line then Some() else None

    match lines with
    | HeaderLine :: Dashes :: rows ->
        rows
        |> List.choose (fun (line: string) ->
            let parts: string array =
                line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

            if parts.Length <= 2 then
                None
            else
                Some
                    {
                        PackageId = parts.[0]
                        Version = parts.[1]
                    }
        )
    | _ -> []

/// The tools of `dotnet tool list --format json`, which carries a `version` of its own and is the
/// SDK naming a shape it intends to keep, rather than a table laid out to be read by a person.
/// `None` for output that is not that, so the caller can fall back to the table.
let toolsFromJson (output: string) : ListedTool list option =
    try
        match JObject.Parse(output).["data"] with
        | :? JArray as data ->
            data
            |> Seq.choose (fun entry ->
                match entry.["packageId"], entry.["version"] with
                | null, _
                | _, null -> None
                | packageId, version ->
                    Some
                        {
                            PackageId = packageId.Value<string>()
                            Version = version.Value<string>()
                        }
            )
            |> List.ofSeq
            |> Some
        | _ -> None
    with _ ->
        None

let toolListStartInfo (workingDir: string) (arguments: string) : ProcessStartInfo =
    let ps = ProcessStartInfo("dotnet")
    ps.WorkingDirectory <- workingDir

    // Only the table is localised, but asking for one language costs nothing on the JSON path and
    // saves having to remember which of the two needed it.
    if ps.EnvironmentVariables.ContainsKey "DOTNET_CLI_UI_LANGUAGE" then
        ps.EnvironmentVariables.["DOTNET_CLI_UI_LANGUAGE"] <- "en-us"
    else
        ps.EnvironmentVariables.Add("DOTNET_CLI_UI_LANGUAGE", "en-us")

    ps.CreateNoWindow <- true
    ps.Arguments <- arguments
    ps.RedirectStandardOutput <- true
    ps.RedirectStandardError <- true
    ps.UseShellExecute <- false
    ps

/// Whether this SDK takes `dotnet tool list --format json`.
[<RequireQualifiedAccess; Struct>]
type JsonToolListSupport =
    /// Nothing has asked yet, so ask.
    | NotAsked
    /// It answered in JSON, and will again.
    | Supported
    /// It refused the flag, as every SDK before 9.0.100 does. Read the table instead, and do not
    /// spend another process finding that out.
    | Unsupported

/// Remembered so that a folder resolved after the first does not pay for the same discovery again.
///
/// One answer for the whole process, even though a `global.json` can pin a different SDK per folder,
/// so this is not strictly a property of the machine. Being wrong in either direction is cheap: a
/// folder whose SDK would have answered in JSON is read from the table instead, which every SDK
/// prints, and a folder whose SDK refuses the flag discovers that and falls back. Neither can name
/// the wrong version, so this buys a process rather than an answer.
let mutable jsonToolListSupport: JsonToolListSupport = JsonToolListSupport.NotAsked

let runToolListCmd (Folder workingDir: Folder) (globalFlag: bool) : Result<ListedTool list, DotNetToolListError> =
    let listArguments: string = if globalFlag then "tool list -g" else "tool list"

    let fromTable () : Result<ListedTool list, DotNetToolListError> =
        let ps: ProcessStartInfo = toolListStartInfo workingDir listArguments

        match startProcess ps with
        | Error err -> Error(DotNetToolListError.ProcessStartError err)
        | Ok p ->

        p.WaitForExit()

        if p.ExitCode = 0 then
            Ok(toolsFromTable (readOutputStreamAsLines p.StandardOutput))
        else

        let error: string = p.StandardError.ReadToEnd()
        Error(DotNetToolListError.ExitCodeNonZero(ps.FileName, ps.Arguments, p.ExitCode, error))

    /// `None` when this SDK would not answer in JSON, which is the caller's cue to read the table.
    /// `--format json` arrived in the 9.0.100 SDK and is refused with a non zero exit before that,
    /// which is also how a genuine failure looks, so the two are not told apart here: the table run
    /// is what decides either way, and it is the answer this package has always used.
    let fromJson () : Result<ListedTool list, DotNetToolListError> option =
        let ps: ProcessStartInfo =
            toolListStartInfo workingDir $"%s{listArguments} --format json"

        match startProcess ps with
        | Error err -> Some(Error(DotNetToolListError.ProcessStartError err))
        | Ok p ->

        p.WaitForExit()

        if p.ExitCode <> 0 then
            None
        else

        readOutputStreamAsLines p.StandardOutput
        |> String.concat ""
        |> toolsFromJson
        |> Option.map Ok

    match jsonToolListSupport with
    | JsonToolListSupport.Unsupported -> fromTable ()
    | JsonToolListSupport.NotAsked
    | JsonToolListSupport.Supported ->

    match fromJson () with
    | Some(Ok tools) ->
        jsonToolListSupport <- JsonToolListSupport.Supported
        Ok tools
    // `dotnet` itself would not start, so the table run would fail the same way. Nothing was
    // learned about the flag, so nothing is remembered about it.
    | Some(Error error) -> Error error
    | None ->
        jsonToolListSupport <- JsonToolListSupport.Unsupported
        fromTable ()

[<return: Struct>]
let (|CompatibleTool|_|) (tools: ListedTool list) : FantomasVersion voption =
    let compatible: ListedTool option =
        tools
        |> List.tryFind (fun tool ->
            match tool.PackageId, tool.Version with
            | CompatibleToolName _, CompatibleVersion _ -> true
            | _ -> false
        )

    match compatible with
    | None -> ValueNone
    | Some tool -> ValueSome(FantomasVersion.Create tool.Version)

let isWindows: bool = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)

/// How long a freshly started daemon has to answer the version handshake before it is given up on.
[<Literal>]
let handshakeTimeoutInMs = 30_000

// Find an executable fantomas file on the PATH
let fantomasVersionOnPath () : (FantomasExecutableFile * FantomasVersion) option =
    let fantomasExecutableOnPathOpt =
        match Option.ofObj (Environment.GetEnvironmentVariable("PATH")) with
        | Some s -> s.Split([| if isWindows then ';' else ':' |], StringSplitOptions.RemoveEmptyEntries)
        | None -> Array.empty
        |> Seq.choose (fun folder ->
            if isWindows then
                let fantomasExe = Path.Combine(folder, "fantomas.exe")

                let fantomasToolExe = Path.Combine(folder, "fantomas-tool.exe")

                if File.Exists fantomasExe then Some fantomasExe
                elif File.Exists fantomasToolExe then Some fantomasToolExe
                else None
            else
                let fantomas = Path.Combine(folder, "fantomas")
                let fantomasTool = Path.Combine(folder, "fantomas-tool")

                if File.Exists fantomas then Some fantomas
                elif File.Exists fantomasTool then Some fantomasTool
                else None
        )
        |> Seq.tryHead

    fantomasExecutableOnPathOpt
    |> Option.bind (fun fantomasExecutablePath ->
        let processStart = ProcessStartInfo(fantomasExecutablePath)
        processStart.Arguments <- "--version"
        processStart.CreateNoWindow <- true
        processStart.RedirectStandardOutput <- true
        processStart.RedirectStandardError <- true
        processStart.UseShellExecute <- false

        match startProcess processStart with
        | Ok p ->
            // Standard error is redirected, so something has to read it, or a version that wrote
            // more to it than the pipe holds would block on the write and never exit. This drains
            // it with no subscriber attached, which is all that is wanted: nothing here reads what
            // it says.
            p.BeginErrorReadLine()

            let stdOut = p.StandardOutput.ReadToEnd()
            p.WaitForExit()

            stdOut
            |> Option.ofObj
            |> Option.map (fun s -> FantomasExecutableFile(fantomasExecutablePath), FantomasVersion.Create s)
        | Error(ProcessStartError.ExecutableFileNotFound _)
        | Error(ProcessStartError.UnExpectedException _) -> None
    )

let findFantomasTool (workingDir: Folder) : Result<FantomasToolFound, FantomasToolError> =
    // First try and find a local tool for the folder.
    // Next see if there is a global tool.
    // Lastly check if an executable `fantomas` is present on the PATH.
    let localToolsListResult = runToolListCmd workingDir false

    match localToolsListResult with
    | Ok(CompatibleTool version) -> Ok(FantomasToolFound(version, FantomasToolStartInfo.LocalTool workingDir))
    | Error err -> Error(FantomasToolError.DotNetListError err)
    | Ok _localToolListResult ->

    let globalToolsListResult = runToolListCmd workingDir true

    match globalToolsListResult with
    | Ok(CompatibleTool version) -> Ok(FantomasToolFound(version, FantomasToolStartInfo.GlobalTool))
    | Error err -> Error(FantomasToolError.DotNetListError err)
    | Ok _nonCompatibleGlobalVersion ->

    let fantomasOnPathVersion = fantomasVersionOnPath ()

    match fantomasOnPathVersion with
    | Some(executableFile, (FantomasVersion(CompatibleVersion _) as version)) ->
        Ok(FantomasToolFound(version, FantomasToolStartInfo.ToolOnPath executableFile))
    | _ -> Error FantomasToolError.NoCompatibleVersionFound

// Fantomas added `fantomas daemon` beside `fantomas --daemon` in 8.0.0-alpha-016, and both do the
// same thing. The flag is kept working so that an older client can start a newer Fantomas, and this
// is the other half of that bargain: a client that knows which version it found asks for the daemon
// the way that version spells it.
//
// The number here is the release the subcommand landed in and has to stay that. Naming a later one
// leaves the alphas on the flag, so the path this decides is never exercised until a stable ships.
// Naming an earlier one tells a Fantomas that has no `daemon` command to use it, and the only
// symptom is an editor reporting that no daemon could be started.
//
// Prereleases count, which is why `includePrerelease` is on. Every version below this one is
// answered with the flag, which works on every Fantomas there has ever been including the newest,
// so being wrong in that direction costs nothing.
//
// One thing this leans on: the alpha number is zero padded to three digits, so `alpha-099` sorts
// below `alpha-100`. Semver compares a non numeric prerelease identifier character by character,
// and without the padding `alpha-9` would sort above `alpha-10`.
let daemonSubcommandRange: SemanticVersioning.Range =
    SemanticVersioning.Range(">=8.0.0-alpha-016")

let daemonArgument (version: FantomasVersion) : string =
    let (FantomasVersion printed) = version

    match SemanticVersioning.Version.TryParse printed with
    | true, parsed when daemonSubcommandRange.IsSatisfied(parsed, includePrerelease = true) -> "daemon"
    | _ -> "--daemon"

let createForVersion
    (version: FantomasVersion)
    (startInfo: FantomasToolStartInfo)
    : Result<RunningFantomasTool, ProcessStartError>
    =
    let daemon: string = daemonArgument version

    let processStart =
        match startInfo with
        | FantomasToolStartInfo.LocalTool(Folder workingDirectory) ->
            let ps = ProcessStartInfo("dotnet")
            ps.WorkingDirectory <- workingDirectory
            ps.Arguments <- $"fantomas %s{daemon}"
            ps
        | FantomasToolStartInfo.GlobalTool ->

            let globalToolsPath =
                match Option.ofObj (Environment.GetEnvironmentVariable("DOTNET_CLI_HOME")) with
                | Some s -> Path.Combine(s, "tools")
                | None ->
                    Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.UserProfile), ".dotnet", "tools")

            let fantomasExecutable =
                let fileName = if isWindows then "fantomas.exe" else "fantomas"
                Path.Combine(globalToolsPath, fileName)

            let ps = ProcessStartInfo(fantomasExecutable)
            ps.Arguments <- daemon
            ps
        | FantomasToolStartInfo.ToolOnPath(FantomasExecutableFile executableFile) ->
            let ps = ProcessStartInfo(executableFile)
            ps.Arguments <- daemon
            ps

    processStart.UseShellExecute <- false
    processStart.RedirectStandardInput <- true
    processStart.RedirectStandardOutput <- true
    processStart.RedirectStandardError <- true
    processStart.CreateNoWindow <- true

    match startProcess processStart with
    | Error err -> Error err
    | Ok daemonProcess ->

    // Standard error is redirected, so something has to read it. Left alone it fills the pipe
    // the operating system gives the two processes, 4KB by default on Windows, and the daemon
    // then blocks inside whatever it was doing when it wrote the line that did not fit. Keep
    // the last of it for the failure message below and throw the rest away.
    let recentStandardError = Queue<string>()

    daemonProcess.ErrorDataReceived.Add(fun message ->
        if not (isNull message.Data) then
            lock
                recentStandardError
                (fun () ->
                    recentStandardError.Enqueue message.Data

                    while recentStandardError.Count > 50 do
                        recentStandardError.Dequeue() |> ignore
                )
    )

    daemonProcess.BeginErrorReadLine()

    let client =
        new JsonRpc(daemonProcess.StandardInput.BaseStream, daemonProcess.StandardOutput.BaseStream)

    let configurationWarnings = Event<ConfigurationWarning>()

    // Has to happen before StartListening, for two reasons: StreamJsonRpc refuses to add local
    // methods once the connection is listening, and a subscriber added before the daemon can
    // send anything is a subscriber that cannot be raced by the first notification. A daemon
    // that never sends these simply never triggers it.
    client.AddLocalRpcMethod(
        Methods.ConfigurationWarning,
        Action<ConfigurationWarning>(fun warning ->
            try
                configurationWarnings.Trigger warning
            with _ ->
                // A subscriber that throws must not take the connection down with it. An
                // exception out of a synchronous local method is not turned into an error
                // response: it escapes the dispatcher and disconnects the client, which would
                // fault every format request from then on over a message that only carries
                // advice.
                ()
        )
    )

    do client.StartListening()

    try
        // Get the version first as a sanity check that connection is possible.
        //
        // Bounded, because this is what every caller waits behind: `LSPFantomasService` resolves
        // daemons on one mailbox, so a process that starts and then never answers would hold up
        // every format request for the rest of the session with nothing to show for it. Generous
        // enough that a cold start on a loaded machine is not mistaken for a hang; a daemon that
        // is still silent by then is one the cleanup below should get its hands on.
        let printedVersion: string =
            client.InvokeAsync<string>(Fantomas.Client.Contracts.Methods.Version)
            |> Async.AwaitTask
            |> fun handshake -> Async.RunSynchronously(handshake, timeout = handshakeTimeoutInMs)

        // What the daemon answered rather than the version it was started for, because `createFor`
        // starts one without knowing which version that is, and a warning has to name the Fantomas
        // that raised it. Read through `FantomasVersion`, so what a consumer shows a user is
        // `8.0.0-alpha-022` rather than the `+<commit>` form `fantomas --version` prints.
        let daemonVersion: string =
            string<FantomasVersion>(FantomasVersion.Create printedVersion)

        Ok
            {
                RpcClient = client
                Process = daemonProcess
                StartInfo = startInfo
                // Stamped as the warning is forwarded, so every one that leaves this package names
                // its daemon whichever way a caller started it. The daemon does not send a version,
                // which means this works against every Fantomas 8 daemon there already is.
                ConfigurationWarnings =
                    configurationWarnings.Publish
                    |> Event.map (fun warning -> { warning with Version = daemonVersion })
            }
    with ex ->
        let error =
            // A timeout says nothing about what was being waited for, so say it here. The next
            // field report should carry a number someone can argue about rather than "it said
            // something timed out".
            if ex :? TimeoutException then
                $"Daemon did not answer the version request within %i{handshakeTimeoutInMs} ms."
            elif daemonProcess.HasExited then
                // `HasExited` only says the process is gone; the handler above is fed
                // asynchronously and can still be behind. `WaitForExit` with no timeout is the
                // one overload that waits for the readers to drain too, so without it the
                // message here would be missing the lines that explain the failure.
                daemonProcess.WaitForExit()

                let stdErr =
                    lock recentStandardError (fun () -> String.Join(Environment.NewLine, recentStandardError))

                $"Daemon std error: %s{stdErr}.\nJsonRpc exception:%s{ex.Message}"
            else
                ex.Message

        // The handshake failed, so nothing will ever hand this daemon out and nothing will
        // ever dispose it. Kill it here rather than leave the process running for the rest of
        // the session with no way to reach it.
        try
            client.Dispose()
        with _ ->
            ()

        try
            if not daemonProcess.HasExited then
                daemonProcess.Kill()

            daemonProcess.Dispose()
        with _ ->
            ()

        Error(ProcessStartError.UnExpectedException(processStart.FileName, processStart.Arguments, error))

// Without a version to go on, ask the way every version there has ever been understands.
let createFor (startInfo: FantomasToolStartInfo) : Result<RunningFantomasTool, ProcessStartError> =
    createForVersion (FantomasVersion "0.0.0") startInfo
