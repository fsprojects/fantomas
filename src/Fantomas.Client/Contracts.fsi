module Fantomas.Client.Contracts

open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

module Methods =

    [<Literal>]
    val Version: string = "fantomas/version"

    [<Literal>]
    val FormatDocument: string = "fantomas/formatDocument"

    [<Literal>]
    val FormatSelection: string = "fantomas/formatSelection"

    [<Literal>]
    val Configuration: string = "fantomas/configuration"

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

type FantomasService =
    inherit System.IDisposable

    abstract ClearCache: unit -> unit

    abstract ConfigurationAsync: filePath: string * ?cancellationToken: CancellationToken -> Task<FantomasResponse>

    abstract FormatDocumentAsync:
        FormatDocumentRequest * ?cancellationToken: CancellationToken -> System.Threading.Tasks.Task<FantomasResponse>

    abstract FormatSelectionAsync:
        FormatSelectionRequest * ?cancellationToken: CancellationToken -> System.Threading.Tasks.Task<FantomasResponse>

    abstract VersionAsync:
        filePath: string * ?cancellationToken: CancellationToken -> System.Threading.Tasks.Task<FantomasResponse>
