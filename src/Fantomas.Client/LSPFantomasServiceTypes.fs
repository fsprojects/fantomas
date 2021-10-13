module Fantomas.Client.LSPFantomasServiceTypes

open System
open Fantomas.Client.Contracts

type FormatResponseCode =
    | Formatted = 1
    | UnChanged = 2
    | Error = 3
    | Ignored = 4

[<RequireQualifiedAccess>]
type FormatSelectionResponse =
    | Formatted of filename: string * formattedContent: string
    | Error of filename: string * formattingError: Exception

    member this.AsFormatResponse() =
        match this with
        | FormatSelectionResponse.Formatted (name, content) ->
            { Code = int FormatResponseCode.Formatted
              FileName = name
              Content = Some content }
        | FormatSelectionResponse.Error (name, ex) ->
            { Code = int FormatResponseCode.Error
              FileName = name
              Content = Some(ex.Message) }

[<RequireQualifiedAccess>]
type FormatDocumentResponse =
    | Formatted of filename: string * formattedContent: string
    | Unchanged of filename: string
    | Error of filename: string * formattingError: string
    | IgnoredFile of filename: string

    member this.AsFormatResponse() =
        match this with
        | FormatDocumentResponse.Formatted (name, content) ->
            { Code = int FormatResponseCode.Formatted
              FileName = name
              Content = Some content }
        | FormatDocumentResponse.Unchanged name ->
            { Code = int FormatResponseCode.UnChanged
              FileName = name
              Content = None }
        | FormatDocumentResponse.Error (name, err) ->
            { Code = int FormatResponseCode.Error
              FileName = name
              Content = Some(err) }
        | FormatDocumentResponse.IgnoredFile name ->
            { Code = int FormatResponseCode.Ignored
              FileName = name
              Content = None }
