module Fantomas.Client.FantomasToolLocator

open System
open System.ComponentModel
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions
open System.Runtime.InteropServices
open StreamJsonRpc
open Fantomas.Client.LSPFantomasServiceTypes

// Only 4.6.0-alpha-004 has daemon capabilities
let private supportedRange = SemanticVersioning.Range(">=v4.6.0-alpha-004")

let private (|CompatibleVersion|_|) (version: string) =
    match SemanticVersioning.Version.TryParse version with
    | true, parsedVersion ->
        if supportedRange.IsSatisfied(parsedVersion, includePrerelease = true) then
            Some version
        else
            None
    | _ -> None

// In the past, fantomas was named fantomas-tool.
let private (|CompatibleToolName|_|) toolName =
    if toolName = "fantomas-tool" || toolName = "fantomas" then
        Some toolName
    else
        None

let private readOutputStreamAsLines (outputStream: StreamReader) : string list =
    let rec readLines (outputStream: StreamReader) (continuation: string list -> string list) =
        let nextLine = outputStream.ReadLine()

        if isNull nextLine then
            continuation []
        else
            readLines outputStream (fun lines -> nextLine :: lines |> continuation)

    readLines outputStream id

let private startProcess (ps: ProcessStartInfo) : Result<Process, ProcessStartError> =
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

let private runToolListCmd (Folder workingDir: Folder) (globalFlag: bool) : Result<string list, DotNetToolListError> =
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

let private (|CompatibleTool|_|) lines =
    let (|HeaderLine|_|) line =
        if Regex.IsMatch(line, @"^Package\sId\s+Version.+$") then
            Some()
        else
            None

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

        Option.map (snd >> FantomasVersion) tool
    | _ -> None

let private isWindows = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)

/// Validates that an executable path is safe to execute
/// Prevents arbitrary code execution by ensuring the file:
/// 1. Exists
/// 2. Is named 'fantomas' or 'fantomas-tool' (with optional .exe)
/// 3. Is not in suspicious locations (temp directories, user input paths)
let private validateExecutablePath (path: string) : bool =
    try
        if String.IsNullOrWhiteSpace path then
            false
        else
            let fileName = Path.GetFileName(path).ToLowerInvariant()
            let directory = Path.GetDirectoryName path

            // Only allow known fantomas executable names
            let validNames =
                [ "fantomas"; "fantomas.exe"; "fantomas-tool"; "fantomas-tool.exe" ]

            let isValidName = List.contains fileName validNames

            // Block suspicious directories
            let suspiciousPatterns =
                [ Path.GetTempPath().ToLowerInvariant()
                  Environment.GetFolderPath(Environment.SpecialFolder.InternetCache).ToLowerInvariant()
                  "\\temp\\"
                  "/tmp/"
                  "\\downloads\\"
                  "/downloads/" ]

            let isSuspiciousLocation =
                suspiciousPatterns
                |> List.exists (fun pattern ->
                    not (String.IsNullOrEmpty pattern)
                    && directory.ToLowerInvariant().Contains pattern)

            // File must exist and not be in suspicious location
            isValidName && File.Exists path && not isSuspiciousLocation
    with _ ->
        false

// Find an executable fantomas file on the PATH
let private fantomasVersionOnPath () : (FantomasExecutableFile * FantomasVersion) option =
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
        // SECURITY: Validate executable path before executing
        if not (validateExecutablePath fantomasExecutablePath) then
            None
        else
            let processStart = ProcessStartInfo(fantomasExecutablePath)
            processStart.Arguments <- "--version"
            processStart.RedirectStandardOutput <- true
            processStart.CreateNoWindow <- true
            processStart.RedirectStandardOutput <- true
            processStart.RedirectStandardError <- true
            processStart.UseShellExecute <- false

            match startProcess processStart with
            | Ok p ->
                p.WaitForExit()
                let stdOut = p.StandardOutput.ReadToEnd()

                stdOut
                |> Option.ofObj
                |> Option.map (fun s ->
                    let version = s.ToLowerInvariant().Replace("fantomas", String.Empty).Trim()
                    FantomasExecutableFile fantomasExecutablePath, FantomasVersion version)
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
            // SECURITY: Validate executable path before starting daemon
            if not (validateExecutablePath executableFile) then
                return
                    Error(
                        ProcessStartError.UnExpectedException(
                            executableFile,
                            "--daemon",
                            "Executable path failed security validation. Only known fantomas executables in trusted locations are allowed."
                        )
                    )

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
        let client =
            new JsonRpc(daemonProcess.StandardInput.BaseStream, daemonProcess.StandardOutput.BaseStream)

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
                  StartInfo = startInfo }
        with ex ->
            let error =
                if daemonProcess.HasExited then
                    let stdErr = daemonProcess.StandardError.ReadToEnd()
                    $"Daemon std error: %s{stdErr}.\nJsonRpc exception:%s{ex.Message}"
                else
                    ex.Message

            Error(ProcessStartError.UnExpectedException(processStart.FileName, processStart.Arguments, error))
    | Error err -> Error err
