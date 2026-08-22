module Fantomas.Client.LSPFantomasServiceTypes

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

    member AsFormatResponse: unit -> FantomasResponse

[<RequireQualifiedAccess; NoComparison>]
type FormatDocumentResponse =
    | Formatted of filename: string * formattedContent: string * cursor: FormatCursorPosition option
    | Unchanged of filename: string
    | Error of filename: string * formattingError: string
    | IgnoredFile of filename: string

[<Struct>]
type FantomasVersion = FantomasVersion of string

[<Struct>]
type FantomasExecutableFile = FantomasExecutableFile of string

[<Struct>]
type Folder = Folder of path: string

[<RequireQualifiedAccess>]
type FantomasToolStartInfo =
    | LocalTool of workingDirectory: Folder
    | GlobalTool
    | ToolOnPath of executableFile: FantomasExecutableFile

[<NoComparison>]
type RunningFantomasTool =
    {
        Process: System.Diagnostics.Process
        RpcClient: StreamJsonRpc.JsonRpc
        StartInfo: FantomasToolStartInfo

        /// Raised when this daemon reports settings it could not act on, on whichever thread its
        /// message arrived on. Subscribed to by `createFor` before the connection starts
        /// listening, so no notification can outrun a subscriber added there. A daemon older than
        /// Fantomas 8 does not send these, so the event simply never fires.
        ConfigurationWarnings: IEvent<ConfigurationWarning>
    }

    interface System.IDisposable

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
