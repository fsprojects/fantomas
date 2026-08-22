module Fantomas.Client.Contracts

open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

[<RequireQualifiedAccess>]
module Methods =

    [<Literal>]
    val Version: string = "fantomas/version"

    [<Literal>]
    val FormatDocument: string = "fantomas/formatDocument"

    [<Literal>]
    val FormatSelection: string = "fantomas/formatSelection"

    [<Literal>]
    val Configuration: string = "fantomas/configuration"

    /// Sent by the daemon to the client after every format request, naming the settings in the
    /// resolved configuration that Fantomas could not act on. Purely informational: the request
    /// still succeeds, using defaults for whatever could not be read.
    [<Literal>]
    val ConfigurationWarning: string = "fantomas/configurationWarning"

[<NoComparison>]
type FormatDocumentRequest =
    {
        SourceCode: string

        /// File path will be used to identify the .editorconfig options
        /// Unless the configuration is passed
        FilePath: string

        /// Overrides the found .editorconfig.
        Config: IReadOnlyDictionary<string, string> option

        /// The current position of the cursor.
        /// Zero-based
        Cursor: FormatCursorPosition option
    }

    member IsSignatureFile: bool

and FormatCursorPosition =
    class
        new: line: int * column: int -> FormatCursorPosition
        val Line: int
        val Column: int
    end

[<NoComparison>]
type FormatSelectionRequest =
    {
        SourceCode: string

        /// File path will be used to identify the .editorconfig options
        /// Unless the configuration is passed
        FilePath: string

        /// Overrides the found .editorconfig.
        Config: IReadOnlyDictionary<string, string> option

        /// Range follows the same semantics of the FSharp Compiler Range type.
        Range: FormatSelectionRange
    }

    member IsSignatureFile: bool

and FormatSelectionRange =
    class
        new: startLine: int * startColumn: int * endLine: int * endColumn: int -> FormatSelectionRange
        val StartLine: int
        val StartColumn: int
        val EndLine: int
        val EndColumn: int
    end

[<NoComparison>]
type FantomasResponse =
    {
        Code: int
        FilePath: string
        Content: string option

        /// The actual range that was used to format a selection.
        /// This can differ from the input selection range if the selection had leading or trailing whitespace.
        SelectedRange: FormatSelectionRange option

        /// Cursor position after formatting.
        /// Zero-based.
        Cursor: FormatCursorPosition option
    }

/// Why a setting in the resolved configuration could not be acted on.
/// Carried as an int rather than a union so it survives the wire.
type ConfigurationProblemCode =
    /// A setting carrying the `fsharp_` prefix that this version of Fantomas does not have.
    | UnknownSetting = 1
    /// A setting Fantomas has, carrying a value it cannot parse. The default was used instead.
    /// Only ever reported for a `fsharp_` setting: the four editorconfig defines itself are
    /// shared with other tools, and the spec gives them values Fantomas has no use for.
    | UnrecognizedValue = 2

/// Where a problematic setting was read from.
type ConfigurationProblemSource =
    /// One of the `.editorconfig` files listed in `ConfigurationWarning.EditorConfigFiles`.
    | EditorConfig = 1
    /// The `Config` dictionary the client sent with the request.
    | Request = 2

[<NoComparison>]
type ConfigurationProblem =
    {
        /// One of `ConfigurationProblemCode`.
        Code: int

        /// One of `ConfigurationProblemSource`.
        Source: int

        /// The editorconfig name of the setting, such as `fsharp_multiline_bracket_style`.
        /// A setting name, never a path.
        Setting: string

        /// The value that could not be parsed, exactly as it was written in the configuration.
        /// Null when `Code` is `UnknownSetting`, because then no value was ever read.
        ///
        /// A plain string rather than an option: an F# option travels as
        /// `{"Case":"Some","Fields":[...]}` and disappears entirely when it is `None`, which is
        /// not something a client written in another language should have to know about.
        /// Use `Option.ofObj` from F#.
        Value: string
    }

[<NoComparison>]
type ConfigurationWarning =
    {
        /// The file that was being formatted, echoed back from the request unchanged.
        /// Absolute, because `Fantomas.Client` rejects a relative path before sending the request.
        FilePath: string

        /// Absolute paths of the `.editorconfig` files that contributed to the configuration.
        /// Empty when no `.editorconfig` applies to `FilePath`.
        ///
        /// Which of these a given problem came from is not knowable, and deliberately not
        /// guessed at: editorconfig merges the whole chain into one set of properties before
        /// Fantomas sees it.
        EditorConfigFiles: string array

        /// Empty when nothing is wrong, which clears anything reported for this file earlier.
        Problems: ConfigurationProblem array
    }

type FantomasService =
    inherit System.IDisposable

    abstract ClearCache: unit -> unit

    /// Raised after every format request, with the settings in the resolved configuration that
    /// Fantomas could not act on. Raised with an empty `Problems` list when there is nothing
    /// wrong, so a subscriber can clear what it reported earlier.
    ///
    /// Only daemons from Fantomas 8.0.0-alpha-014 onwards send these; an older one never raises it.
    abstract ConfigurationWarnings: IEvent<ConfigurationWarning>

    abstract ConfigurationAsync: filePath: string * ?cancellationToken: CancellationToken -> Task<FantomasResponse>

    abstract FormatDocumentAsync:
        FormatDocumentRequest * ?cancellationToken: CancellationToken -> System.Threading.Tasks.Task<FantomasResponse>

    abstract FormatSelectionAsync:
        FormatSelectionRequest * ?cancellationToken: CancellationToken -> System.Threading.Tasks.Task<FantomasResponse>

    abstract VersionAsync:
        filePath: string * ?cancellationToken: CancellationToken -> System.Threading.Tasks.Task<FantomasResponse>
