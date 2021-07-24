module Fantomas.Client.Contracts

open System
open System.Collections.Generic
open System.Threading

[<RequireQualifiedAccess>]
module Methods =
    [<Literal>]
    let Version = "fantomas/version"

    [<Literal>]
    let FormatDocument = "fantomas/formatDocument"

    [<Literal>]
    let Configuration = "fantomas/configuration"

type FormatDocumentRequest =
    { SourceCode: string
      /// File path will be used to identify the .editorconfig options
      /// Unless the configuration is passed
      FilePath: string
      /// Overrides the found .editorconfig.
      Config: IReadOnlyDictionary<string, string> option }

type FormatSourceRange =
    class
    end

type FantomasOption = { Type: string; DefaultValue: string }

type ConfigurationResponse =
    { Options: IReadOnlyDictionary<string, FantomasOption>
      EnumOptions: IReadOnlyDictionary<string, string array> }

type VersionResponse = { Version: string }

type FormatDocumentResponse = { Formatted: string }

type FantomasService =
    interface
        inherit IDisposable
        abstract member Version : CancellationToken option -> Async<VersionResponse>

        abstract member FormatDocumentAsync :
            FormatDocumentRequest * CancellationToken option -> Async<FormatDocumentResponse>

        abstract member Configuration : CancellationToken option -> Async<ConfigurationResponse>
    end
