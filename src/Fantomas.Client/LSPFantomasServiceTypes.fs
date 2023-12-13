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

[<RequireQualifiedAccess; NoComparison>]
type FormatSelectionResponse =
    | Formatted of filename: string * formattedContent: string * formattedRange: FormatSelectionRange
    | Error of filename: string * formattingError: string

    member this.AsFormatResponse() =
        match this with
        | FormatSelectionResponse.Formatted(name, content, formattedRange) ->
            { Code = int FantomasResponseCode.Formatted
              FilePath = name
              Content = Some content
              SelectedRange = Some formattedRange
              Cursor = None }
        | FormatSelectionResponse.Error(name, ex) ->
            { Code = int FantomasResponseCode.Error
              FilePath = name
              Content = Some ex
              SelectedRange = None
              Cursor = None }

[<RequireQualifiedAccess; NoComparison>]
type FormatDocumentResponse =
    | Formatted of filename: string * formattedContent: string * cursor: FormatCursorPosition option
    | Unchanged of filename: string
    | Error of filename: string * formattingError: string
    | IgnoredFile of filename: string

type FantomasVersion = FantomasVersion of string
type FantomasExecutableFile = FantomasExecutableFile of string
type Folder = Folder of path: string

[<RequireQualifiedAccess>]
type FantomasToolStartInfo =
    | LocalTool of workingDirectory: Folder
    | GlobalTool
    | ToolOnPath of executableFile: FantomasExecutableFile

[<NoComparison>]
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
