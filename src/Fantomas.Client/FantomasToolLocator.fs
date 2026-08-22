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
    | Ok p ->
        p.WaitForExit()
        let exitCode = p.ExitCode

        if exitCode = 0 then
            let output = readOutputStreamAsLines p.StandardOutput
            Ok output
        else
            let error = p.StandardError.ReadToEnd()
            Error(DotNetToolListError.ExitCodeNonZero(ps.FileName, ps.Arguments, exitCode, error))
    | Error err -> Error(DotNetToolListError.ProcessStartError err)

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
                    None)

        if List.isEmpty tools then None else Some tools

    match lines with
    | HeaderLine :: Dashes :: Tools tools ->
        let tool =
            List.tryFind
                (fun (packageId, version) ->
                    match packageId, version with
                    | CompatibleToolName _, CompatibleVersion _ -> true
                    | _ -> false)
                tools

        tool |> ValueOption.ofOption |> ValueOption.map (snd >> FantomasVersion)
    | _ -> ValueNone

let isWindows = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)

/// The version `fantomas --version` printed, as `dotnet tool list` would have written it.
///
/// `--version` answers `Fantomas v8.0.0-alpha-014+e4a1c9d...`, `dotnet tool list` answers
/// `8.0.0-alpha-014`. Daemons are cached by this string, so both the leading `v` and the commit
/// hash have to go: with either of them left on, the same Fantomas resolved once from the manifest
/// and once from the PATH counts as two versions and gets two processes.
let normalizeVersion (printed: string) : string =
    let dropPrefix (prefix: string) (text: string) : string =
        if text.StartsWith(prefix, StringComparison.OrdinalIgnoreCase) then
            text.Substring(prefix.Length).Trim()
        else
            text

    let printed = printed.Trim() |> dropPrefix "fantomas" |> dropPrefix "v"

    match printed.IndexOf('+') with
    | -1 -> printed
    | index -> printed.Substring(0, index)

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
                else None)
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
            // more to it than the pipe holds would block on the write and never exit. Nothing here
            // wants what it says.
            p.ErrorDataReceived.Add(ignore)
            p.BeginErrorReadLine()

            let stdOut = p.StandardOutput.ReadToEnd()
            p.WaitForExit()

            stdOut
            |> Option.ofObj
            |> Option.map (fun s ->
                FantomasExecutableFile(fantomasExecutablePath), FantomasVersion(normalizeVersion s))
        | Error(ProcessStartError.ExecutableFileNotFound _)
        | Error(ProcessStartError.UnExpectedException _) -> None)

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

let createFor (startInfo: FantomasToolStartInfo) : Result<RunningFantomasTool, ProcessStartError> =
    let processStart =
        match startInfo with
        | FantomasToolStartInfo.LocalTool(Folder workingDirectory) ->
            let ps = ProcessStartInfo("dotnet")
            ps.WorkingDirectory <- workingDirectory
            ps.Arguments <- "fantomas --daemon"
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
            ps.Arguments <- "--daemon"
            ps
        | FantomasToolStartInfo.ToolOnPath(FantomasExecutableFile executableFile) ->
            let ps = ProcessStartInfo(executableFile)
            ps.Arguments <- "--daemon"
            ps

    processStart.UseShellExecute <- false
    processStart.RedirectStandardInput <- true
    processStart.RedirectStandardOutput <- true
    processStart.RedirectStandardError <- true
    processStart.CreateNoWindow <- true

    match startProcess processStart with
    | Ok daemonProcess ->
        // Standard error is redirected, so something has to read it. Left alone it fills the pipe
        // the operating system gives the two processes, 4KB by default on Windows, and the daemon
        // then blocks inside whatever it was doing when it wrote the line that did not fit. Keep
        // the last of it for the failure message below and throw the rest away.
        let recentStandardError = Queue<string>()

        daemonProcess.ErrorDataReceived.Add(fun message ->
            if not (isNull message.Data) then
                lock recentStandardError (fun () ->
                    recentStandardError.Enqueue message.Data

                    while recentStandardError.Count > 50 do
                        recentStandardError.Dequeue() |> ignore))

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
                    ())
        )

        do client.StartListening()

        try
            // Get the version first as a sanity check that connection is possible
            let _version =
                client.InvokeAsync<string>(Fantomas.Client.Contracts.Methods.Version)
                |> Async.AwaitTask
                |> Async.RunSynchronously

            Ok
                { RpcClient = client
                  Process = daemonProcess
                  StartInfo = startInfo
                  ConfigurationWarnings = configurationWarnings.Publish }
        with ex ->
            let error =
                if daemonProcess.HasExited then
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
    | Error err -> Error err
