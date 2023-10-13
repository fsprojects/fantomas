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
    { SourceCode: string
      FilePath: string
      Config: IReadOnlyDictionary<string, string> option
      Cursor: FormatCursorPosition option }

    member this.IsSignatureFile = this.FilePath.EndsWith(".fsi", StringComparison.Ordinal)

and FormatCursorPosition =
    class
        val Line: int
        val Column: int

        new(line: int, column: int) = { Line = line; Column = column }
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

    member this.IsSignatureFile = this.FilePath.EndsWith(".fsi", StringComparison.Ordinal)

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
    { Code: int
      FilePath: string
      Content: string option
      SelectedRange: FormatSelectionRange option
      Cursor: FormatCursorPosition option }

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
