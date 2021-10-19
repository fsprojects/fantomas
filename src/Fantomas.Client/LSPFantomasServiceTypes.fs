module Fantomas.Client.LSPFantomasServiceTypes

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

[<RequireQualifiedAccess>]
type FormatSelectionResponse =
    | Formatted of filename: string * formattedContent: string
    | Error of filename: string * formattingError: string

    member this.AsFormatResponse() =
        match this with
        | FormatSelectionResponse.Formatted (name, content) ->
            { Code = int FantomasResponseCode.Formatted
              FilePath = name
              Content = Some content }
        | FormatSelectionResponse.Error (name, ex) ->
            { Code = int FantomasResponseCode.Error
              FilePath = name
              Content = Some ex }

[<RequireQualifiedAccess>]
type FormatDocumentResponse =
    | Formatted of filename: string * formattedContent: string
    | Unchanged of filename: string
    | Error of filename: string * formattingError: string
    | IgnoredFile of filename: string

    member this.AsFormatResponse() =
        match this with
        | FormatDocumentResponse.Formatted (name, content) ->
            { Code = int FantomasResponseCode.Formatted
              FilePath = name
              Content = Some content }
        | FormatDocumentResponse.Unchanged name ->
            { Code = int FantomasResponseCode.UnChanged
              FilePath = name
              Content = None }
        | FormatDocumentResponse.Error (name, err) ->
            { Code = int FantomasResponseCode.Error
              FilePath = name
              Content = Some(err) }
        | FormatDocumentResponse.IgnoredFile name ->
            { Code = int FantomasResponseCode.Ignored
              FilePath = name
              Content = None }

type FantomasVersion = FantomasVersion of string

type Folder = Folder of path: string

type ServiceState =
    { Daemons: Map<FantomasVersion, JsonRpc>
      FolderToVersion: Map<Folder, FantomasVersion> }

    static member Empty: ServiceState =
        { Daemons = Map.empty
          FolderToVersion = Map.empty }

type FantomasToolResult =
    | FoundLocalTool of (Folder * FantomasVersion)
    | FoundGlobalTool of (Folder * FantomasVersion)
    | NoCompatibleVersionFound
