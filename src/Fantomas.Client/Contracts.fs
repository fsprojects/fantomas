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
      /// File path will be used to identify the .editorconfig options
      /// Unless the configuration is passed
      FilePath: string
      /// Determines the underlying F# ParsingOptions
      IsLastFile: bool
      /// Overrides the found .editorconfig.
      Config: IReadOnlyDictionary<string, string> option }

type FormatResponse =
    { Code: int
      FileName: string
      Content: string option }

type FormatSelectionRequest =
    { SourceCode: string
      /// File path will be used to identify the .editorconfig options
      /// Unless the configuration is passed
      FilePath: string
      /// Overrides the found .editorconfig.
      Config: IReadOnlyDictionary<string, string> option
      /// Range follows the same semantics of the FSharp Compiler Range type.
      Range: FormatSelectionRange }

and FormatSelectionRange =
    struct
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

type FantomasOption = { Type: string; DefaultValue: string }

type ConfigurationResponse =
    { Options: IReadOnlyDictionary<string, FantomasOption>
      EnumOptions: IReadOnlyDictionary<string, string array> }

type VersionResponse = { Version: string }

type FantomasService =
    interface
        inherit IDisposable
        abstract member VersionAsync : ?cancellationToken: CancellationToken -> Task<VersionResponse>

        abstract member FormatDocumentAsync :
            FormatDocumentRequest * ?cancellationToken: CancellationToken -> Task<FormatResponse>

        abstract member FormatSelectionAsync :
            FormatSelectionRequest * ?cancellationToken: CancellationToken -> Task<FormatResponse>

        abstract member ConfigurationAsync : ?cancellationToken: CancellationToken -> Task<ConfigurationResponse>
    end
