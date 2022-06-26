module Fantomas.Client.LSPFantomasServiceTypes

open System
open System.Diagnostics
open StreamJsonRpc
open Fantomas.Client.Contracts

type FantomasResponseCode =
    | Formatted = 1
    | UnChanged = 2
    | Error = 3
    | Ignored = 4
    | Version = 5
    | ToolNotFound = 6
    | FileNotFound = 7
    | Configuration = 8
    | FilePathIsNotAbsolute = 9
    | CancellationWasRequested = 10
    | DaemonCreationFailed = 11

[<RequireQualifiedAccess>]
type FormatSelectionResponse =
    | Formatted of filename: string * formattedContent: string * formattedRange: FormatSelectionRange
    | Error of filename: string * formattingError: string

    member this.AsFormatResponse() =
        match this with
        | FormatSelectionResponse.Formatted (name, content, formattedRange) ->
            { Code = int FantomasResponseCode.Formatted
              FilePath = name
              Content = Some content
              SelectedRange = Some formattedRange }
        | FormatSelectionResponse.Error (name, ex) ->
            { Code = int FantomasResponseCode.Error
              FilePath = name
              Content = Some ex
              SelectedRange = None }

[<RequireQualifiedAccess>]
type FormatDocumentResponse =
    | Formatted of filename: string * formattedContent: string
    | Unchanged of filename: string
    | Error of filename: string * formattingError: string
    | IgnoredFile of filename: string

    member this.AsFormatResponse() =
        match this with
        | FormatDocumentResponse.Formatted (name, content) ->
            { Code = int FantomasResponseCode.Formatted
              FilePath = name
              Content = Some content
              SelectedRange = None }
        | FormatDocumentResponse.Unchanged name ->
            { Code = int FantomasResponseCode.UnChanged
              FilePath = name
              Content = None
              SelectedRange = None }
        | FormatDocumentResponse.Error (name, err) ->
            { Code = int FantomasResponseCode.Error
              FilePath = name
              Content = Some(err)
              SelectedRange = None }
        | FormatDocumentResponse.IgnoredFile name ->
            { Code = int FantomasResponseCode.Ignored
              FilePath = name
              Content = None
              SelectedRange = None }

type FantomasVersion = FantomasVersion of string
type FantomasExecutableFile = FantomasExecutableFile of string
type Folder = Folder of path: string

[<RequireQualifiedAccess>]
type FantomasToolStartInfo =
    | LocalTool of workingDirectory: Folder
    | GlobalTool
    | ToolOnPath of executableFile: FantomasExecutableFile

type RunningFantomasTool =
    { Process: Process
      RpcClient: JsonRpc
      StartInfo: FantomasToolStartInfo }

    interface IDisposable with
        member this.Dispose() : unit =
            if not this.Process.HasExited then
                this.Process.Kill()

            this.Process.Dispose()
            this.RpcClient.Dispose()

type ServiceState =
    { Daemons: Map<FantomasVersion, RunningFantomasTool>
      FolderToVersion: Map<Folder, FantomasVersion> }

    static member Empty: ServiceState =
        { Daemons = Map.empty
          FolderToVersion = Map.empty }

[<RequireQualifiedAccess>]
type ProcessStartError =
    | ExecutableFileNotFound of
        executableFile: string *
        arguments: string *
        workingDirectory: string *
        pathEnvironmentVariable: string *
        error: string
    | UnExpectedException of executableFile: string * arguments: string * error: string

[<RequireQualifiedAccess>]
type DotNetToolListError =
    | ProcessStartError of ProcessStartError
    | ExitCodeNonZero of executableFile: string * arguments: string * exitCode: int * error: string

type FantomasToolFound = FantomasToolFound of version: FantomasVersion * startInfo: FantomasToolStartInfo

[<RequireQualifiedAccess>]
type FantomasToolError =
    | NoCompatibleVersionFound
    | DotNetListError of DotNetToolListError

[<RequireQualifiedAccess>]
type GetDaemonError =
    | DotNetToolListError of error: DotNetToolListError
    | FantomasProcessStart of error: ProcessStartError
    | InCompatibleVersionFound
    | CompatibleVersionIsKnownButNoDaemonIsRunning of version: FantomasVersion
