module internal Fantomas.Client.FantomasToolLocator

open System
open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

type FantomasToolResult =
    | FoundLocalTool
    | FoundGlobalTool
    | NoCompatibleVersionFound

let private (|CompatibleVersion|_|) (version: string) =
    let stripAlphaBeta = version.Split('-').[0]

    match Version.TryParse stripAlphaBeta with
    | true, version when version.Major >= 4 && version.Minor >= 6 -> Some version
    | _ -> None

// In the future, fantomas-tool will be renamed to fantomas.
let private (|CompatibleToolName|_|) toolName =
    if toolName = "fantomas-tool"
       || toolName = "fantomas" then
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

let private runToolListCmd (workingDir: string) (globalFlag: bool) =
    let ps = ProcessStartInfo("dotnet")
    ps.WorkingDirectory <- workingDir

    ps.Arguments <-
        if globalFlag then
            "tool list -g"
        else
            "tool list"

    ps.RedirectStandardOutput <- true
    ps.RedirectStandardError <- true
    ps.UseShellExecute <- false
    use p = Process.Start ps
    p.WaitForExit()
    let exitCode = p.ExitCode

    if exitCode = 0 then
        let output = readOutputStreamAsLines p.StandardOutput
        Ok output
    else
        let error = p.StandardError.ReadToEnd()
        Error(exitCode, error)

let private (|CompatibleTool|_|) lines =
    let (|HeaderLine|_|) line =
        if Regex.IsMatch(line, @"^Package\sId\s+Version.+$") then
            Some()
        else
            None

    let (|Dashes|_|) line =
        if String.forall ((=) '-') line then
            Some()
        else
            None

    let (|Tools|_|) lines =
        let tools =
            lines
            |> List.choose
                (fun (line: string) ->
                    let parts =
                        line.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)

                    if parts.Length > 2 then
                        Some(parts.[0], parts.[1])
                    else
                        None)

        if List.isEmpty tools then
            None
        else
            Some tools

    match lines with
    | HeaderLine :: Dashes :: Tools tools ->
        let hasTool =
            List.exists
                (fun (packageId, version) ->
                    match packageId, version with
                    | CompatibleToolName _, CompatibleVersion _ -> true
                    | _ -> false)
                tools

        if hasTool then Some() else None
    | _ -> None

let private tryFindGlobalVersion () : FantomasToolResult =
    // check if fantomas is available on the Path environment variable
    failwith "todo!"

let findFantomasTool (workingDir: string) : FantomasToolResult =
    // check if there is a dotnet-tools.json manifest
    let localTools = runToolListCmd workingDir false

    match localTools with
    | Ok CompatibleTool -> FoundLocalTool
    | _ ->
        let globalTools = runToolListCmd workingDir true

        match globalTools with
        | Ok CompatibleTool -> FoundGlobalTool
        | _ -> NoCompatibleVersionFound
