module Fantomas.Client.Contracts

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

[<RequireQualifiedAccess>]
module Methods =
    [<Literal>]
    let Version = "fantomas/version"

    [<Literal>]
    let FormatDocument = "fantomas/formatDocument"

    [<Literal>]
    let FormatSelection = "fantomas/formatSelection"

    [<Literal>]
    let Configuration = "fantomas/configuration"

type FormatDocumentRequest =
    {
        SourceCode: string
        /// File path will be used to identify the .editorconfig options
        /// Unless the configuration is passed
        FilePath: string
        /// Overrides the found .editorconfig.
        Config: IReadOnlyDictionary<string, string> option
    }

    member this.IsSignatureFile = this.FilePath.EndsWith(".fsi")

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

    member this.IsSignatureFile = this.FilePath.EndsWith(".fsi")

and FormatSelectionRange =
    class
        val StartLine: int
        val StartColumn: int
        val EndLine: int
        val EndColumn: int

        new(startLine: int, startColumn: int, endLine: int, endColumn: int) =
            { StartLine = startLine
              StartColumn = startColumn
              EndLine = endLine
              EndColumn = endColumn }
    end

type FantomasResponse =
    {
        Code: int
        FilePath: string
        Content: string option
        /// The actual range that was used to format a selection.
        /// This can differ from the input selection range if the selection had leading or trailing whitespace.
        SelectedRange: FormatSelectionRange option
    }

type FantomasService =
    interface
        inherit IDisposable

        abstract member VersionAsync: filePath: string * ?cancellationToken: CancellationToken -> Task<FantomasResponse>

        abstract member FormatDocumentAsync:
            FormatDocumentRequest * ?cancellationToken: CancellationToken -> Task<FantomasResponse>

        abstract member FormatSelectionAsync:
            FormatSelectionRequest * ?cancellationToken: CancellationToken -> Task<FantomasResponse>

        abstract member ConfigurationAsync:
            filePath: string * ?cancellationToken: CancellationToken -> Task<FantomasResponse>

        abstract member ClearCache: unit -> unit
    end
