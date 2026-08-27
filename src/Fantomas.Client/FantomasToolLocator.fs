module Fantomas.Client.FantomasToolLocator

open System
open System.Collections.Generic
open System.ComponentModel
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open System.Runtime.InteropServices
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

let runToolListCmd (Folder workingDir: Folder) (globalFlag: bool) : Result<string list, DotNetToolListError> =
    let ps = ProcessStartInfo("dotnet")
    ps.WorkingDirectory <- workingDir

    if ps.EnvironmentVariables.ContainsKey "DOTNET_CLI_UI_LANGUAGE" then
        ps.EnvironmentVariables.["DOTNET_CLI_UI_LANGUAGE"] <- "en-us"
    else
        ps.EnvironmentVariables.Add("DOTNET_CLI_UI_LANGUAGE", "en-us")

    ps.CreateNoWindow <- true
    ps.Arguments <- if globalFlag then "tool list -g" else "tool list"
    ps.RedirectStandardOutput <- true
    ps.RedirectStandardError <- true
    ps.UseShellExecute <- false

    match startProcess ps with
    | Error err -> Error(DotNetToolListError.ProcessStartError err)
    | Ok p ->
        p.WaitForExit()
        let exitCode = p.ExitCode

        if exitCode = 0 then
            let output = readOutputStreamAsLines p.StandardOutput
            Ok output
        else
            let error = p.StandardError.ReadToEnd()
            Error(DotNetToolListError.ExitCodeNonZero(ps.FileName, ps.Arguments, exitCode, error))

let packageSidVersionRegex = Regex(@"^Package\sId\s+Version.+$")

[<return: Struct>]
let (|CompatibleTool|_|) (lines: string list) : FantomasVersion voption =
    // These are local bindings, which cannot carry an attribute, so they stay on option.
    let (|HeaderLine|_|) line =
        if packageSidVersionRegex.IsMatch line then Some() else None

    let (|Dashes|_|) line =
        if String.forall ((=) '-') line then Some() else None

    let (|Tools|_|) lines =
        let tools =
            lines
            |> List.choose (fun (line: string) ->
                let parts = line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

                if parts.Length > 2 then
                    Some(parts.[0], parts.[1])
                else
                    None
            )

        if List.isEmpty tools then None else Some tools

    match lines with
    | HeaderLine :: Dashes :: Tools tools ->
        let tool: (string * string) option =
            List.tryFind
                (fun (packageId, version) ->
                    match packageId, version with
                    | CompatibleToolName _, CompatibleVersion _ -> true
                    | _ -> false
                )
                tools

        // Folded to match `normalizeVersion`. Daemons are cached under this string and the two
        // producers are compared as plain strings, so both sides have to normalise the same way.
        match tool with
        | None -> ValueNone
        | Some(_, version) -> ValueSome(FantomasVersion(version.ToLowerInvariant()))
    | _ -> ValueNone

let isWindows = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)

let normalizeVersion (printed: string) : string =
    let dropPrefix (prefix: string) (text: string) : string =
        if text.StartsWith(prefix, StringComparison.OrdinalIgnoreCase) then
            text.Substring(prefix.Length).Trim()
        else
            text

    let printed: string = printed.Trim() |> dropPrefix "fantomas" |> dropPrefix "v"

    let buildMetadata: int = printed.IndexOf('+')

    let withoutBuildMetadata: string =
        if buildMetadata = -1 then
            printed
        else
            printed.Substring(0, buildMetadata)

    // Folded, and folded on the other side too, in `CompatibleTool`. Doing it here alone would be
    // worse than not doing it: the two producers have to agree, and they are compared as plain
    // strings. Semver reads prerelease identifiers case sensitively, so this is a deliberate
    // decision that `8.0.0-Alpha-014` and `8.0.0-alpha-014` name one Fantomas, which is true of
    // every package either path can resolve.
    withoutBuildMetadata.ToLowerInvariant()

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
            |> Option.map (fun s ->
                FantomasExecutableFile(fantomasExecutablePath), FantomasVersion(normalizeVersion s)
            )
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
    | Some(executableFile, FantomasVersion(CompatibleVersion version)) ->
        Ok(FantomasToolFound((FantomasVersion(version)), FantomasToolStartInfo.ToolOnPath executableFile))
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
            let _version =
                client.InvokeAsync<string>(Fantomas.Client.Contracts.Methods.Version)
                |> Async.AwaitTask
                |> fun handshake -> Async.RunSynchronously(handshake, timeout = handshakeTimeoutInMs)

            Ok
                {
                    RpcClient = client
                    Process = daemonProcess
                    StartInfo = startInfo
                    ConfigurationWarnings = configurationWarnings.Publish
                }
        with ex ->
            let error =
                // A timeout says nothing about what was being waited for, so say it here. The next
                // field report should carry a number someone can argue about rather than "it said
                // something timed out".
                if (ex :? TimeoutException) then
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
