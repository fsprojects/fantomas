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

/// How much a log message matters, for a host to route on. A real enum rather than an int: this
/// never travels over the wire, unlike `FantomasResponseCode` and `ConfigurationProblemCode`.
type FantomasLogLevel =
    | Debug = 0
    | Info = 1
    | Warning = 2
    | Error = 3

/// A Fantomas version in the one shape daemons are cached under: as `dotnet tool list` writes it,
/// with no `Fantomas` in front, no leading `v`, no `+<commit>` on the end, and folded to lower case.
///
/// The case is internal so that every value comes out of `FantomasToolLocator.findFantomasTool`,
/// which is what makes that shape a fact rather than a hope. The cache compares these as plain
/// strings, so the same Fantomas resolved once from a tool manifest and once from the PATH has to
/// arrive as the same text, or it gets two daemon processes.
[<Struct>]
type FantomasVersion =
    internal
    | FantomasVersion of string

    /// The version as `dotnet tool list` writes it, such as `8.0.0-alpha-020`. For display only:
    /// nothing in this API takes a version string.
    override ToString: unit -> string

    /// Read a version the way whichever producer wrote it and fold it to the one cached shape.
    /// `fantomas --version` answers `Fantomas v8.0.0-alpha-020+e4a1c9d`, `dotnet tool list` answers
    /// `8.0.0-alpha-020`, and both have to arrive here as the latter.
    static member internal Create: printed: string -> FantomasVersion

[<Struct>]
type FantomasExecutableFile = | FantomasExecutableFile of string

[<Struct>]
type Folder = | Folder of path: string

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

type FantomasToolFound = | FantomasToolFound of version: FantomasVersion * startInfo: FantomasToolStartInfo

[<RequireQualifiedAccess>]
type FantomasToolError =
    | NoCompatibleVersionFound
    | DotNetListError of DotNetToolListError
