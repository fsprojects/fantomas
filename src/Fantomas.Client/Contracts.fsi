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

    /// Sent by the daemon to the client for every format request, before the request answers,
    /// naming the settings in the resolved configuration that Fantomas could not act on. Purely
    /// informational: the request still succeeds, using defaults for whatever could not be read.
    ///
    /// The notification carries one `ConfigurationWarning`, serialized as it is written here
    /// except for `Version`, which the daemon does not send:
    /// `{"FilePath": "...", "EditorConfigFiles": ["..."], "Problems": [{"Code": 1, "Source": 1,
    /// "Setting": "fsharp_bogus_option", "Value": null}]}`.
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
        /// The Fantomas that raised this, in the shape `dotnet tool list` writes: no `Fantomas` in
        /// front, no leading `v`, no `+<commit>` on the end, such as `8.0.0-alpha-022`. Ready to
        /// put in front of a user, which is the one thing this is here for.
        ///
        /// Filled in by `Fantomas.Client` from the daemon that raised the warning, rather than
        /// sent by the daemon: that is what lets it name every Fantomas 8 daemon there already is
        /// rather than only the ones released after this. Talking to a daemon yourself, this
        /// arrives as `null` and you already know which version you started.
        Version: string

        /// The file that was being formatted, echoed back from the request unchanged.
        /// Absolute, because `Fantomas.Client` rejects a relative path before sending the request.
        FilePath: string

        /// Absolute paths of the `.editorconfig` files that contributed to the configuration.
        /// Empty when `Problems` is empty, and when no `.editorconfig` applies to `FilePath`:
        /// it is the same list on every request for a file, and there is nothing to do with it
        /// while nothing is wrong.
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

    /// Raised for every format request, with the settings in the resolved configuration that
    /// Fantomas could not act on. Raised with an empty `Problems` list when there is nothing
    /// wrong, so a subscriber can clear what it reported earlier, and raised before the request it
    /// belongs to answers.
    ///
    /// Raised on whichever thread the daemon's answer arrived on, never on the caller's. A
    /// subscriber that touches a UI has to marshal, and one that throws is swallowed rather than
    /// allowed to fault the connection.
    ///
    /// Warnings for one file arrive in the order the requests for it were made, however many of
    /// them you have in flight: the daemon serves one request at a time per file. That is what
    /// makes the clearing rule sound, so an empty one never overtakes problems that are still
    /// current.
    ///
    /// Requests for different files are served concurrently and their warnings interleave freely.
    /// `FilePath` is what tells them apart.
    ///
    /// Only Fantomas 8 daemons send these; an older one never raises it, so no version check is
    /// needed.
    ///
    /// `Version` names the daemon that raised the warning, so reporting one needs no separate
    /// `VersionAsync` call. Asking afterwards would also be a race against `ClearCache`, which can
    /// leave a newly installed daemon answering for a warning the one it replaced raised.
    abstract ConfigurationWarnings: IEvent<ConfigurationWarning>

    abstract ConfigurationAsync: filePath: string * ?cancellationToken: CancellationToken -> Task<FantomasResponse>

    abstract FormatDocumentAsync:
        FormatDocumentRequest * ?cancellationToken: CancellationToken -> System.Threading.Tasks.Task<FantomasResponse>

    abstract FormatSelectionAsync:
        FormatSelectionRequest * ?cancellationToken: CancellationToken -> System.Threading.Tasks.Task<FantomasResponse>

    abstract VersionAsync:
        filePath: string * ?cancellationToken: CancellationToken -> System.Threading.Tasks.Task<FantomasResponse>
