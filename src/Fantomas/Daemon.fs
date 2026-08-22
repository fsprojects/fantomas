module Fantomas.Daemon

open System
open System.Diagnostics
open System.IO
open System.IO.Abstractions
open System.Text.Json
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open StreamJsonRpc
open Fantomas.FCS.Text
open Fantomas.Client.Contracts
open Fantomas.Client.LSPFantomasServiceTypes
open Fantomas.Core
open Fantomas.EditorConfig
open Serilog

[<NoComparison; NoEquality>]
type DaemonEnvironment =
    { FileSystem: IFileSystem
      ReadConfiguration: string -> FormatConfig
      Log: ILogger }

type FantomasDaemon(sender: Stream, reader: Stream, environment: DaemonEnvironment) as this =
    let rpc: JsonRpc = JsonRpc.Attach(sender, reader, this)
    let traceListener = new DefaultTraceListener()

    do
        // hook up request/response logging for debugging
        rpc.TraceSource <- TraceSource(typeof<FantomasDaemon>.Name, SourceLevels.Verbose)
        rpc.TraceSource.Listeners.Add traceListener |> ignore<int>

    let disconnectEvent = new ManualResetEvent(false)

    let exit () = disconnectEvent.Set() |> ignore

    let fs: IFileSystem = environment.FileSystem

    do rpc.Disconnected.Add(fun _ -> exit ())

    interface IDisposable with
        member this.Dispose() =
            traceListener.Dispose()
            disconnectEvent.Dispose()

    member this.WaitForClose = rpc.Completion

    [<JsonRpcMethod(Methods.Version)>]
    member _.Version() : string = CodeFormatter.GetVersion()

    [<JsonRpcMethod(Methods.FormatDocument, UseSingleObjectParameterDeserialization = true)>]
    member _.FormatDocumentAsync(request: FormatDocumentRequest) : Task<FormatDocumentResponse> =
        task {
            if
                IgnoreFile.isIgnoredFile
                    environment.Log
                    (IgnoreFile.find fs (IgnoreFile.loadIgnoreList fs) request.FilePath)
                    request.FilePath
            then
                return FormatDocumentResponse.IgnoredFile request.FilePath
            else
                let config =
                    match request.Config with
                    | Some configProperties ->
                        let config = environment.ReadConfiguration request.FilePath
                        parseOptionsFromEditorConfig config configProperties
                    | None -> environment.ReadConfiguration request.FilePath

                let cursor =
                    request.Cursor
                    |> Option.map (fun cursor -> CodeFormatter.MakePosition(cursor.Line, cursor.Column))

                try
                    let! formatResponse =
                        match cursor with
                        | None -> CodeFormatter.FormatDocumentAsync(request.IsSignatureFile, request.SourceCode, config)
                        | Some cursor ->
                            CodeFormatter.FormatDocumentAsync(
                                request.IsSignatureFile,
                                request.SourceCode,
                                config,
                                cursor
                            )

                    if formatResponse.Code = request.SourceCode then
                        return FormatDocumentResponse.Unchanged request.FilePath
                    else
                        let cursor =
                            formatResponse.Cursor
                            |> Option.map (fun cursorPos -> FormatCursorPosition(cursorPos.Line, cursorPos.Column))

                        return FormatDocumentResponse.Formatted(request.FilePath, formatResponse.Code, cursor)
                with ex ->
                    // A ParseException's own Message is an %A dump of the diagnostic records, and
                    // it is the editor's user who would have been shown it.
                    let message =
                        Diagnostics.describeParseFailure request.FilePath (fun () -> request.SourceCode) ex
                        |> Option.defaultValue ex.Message

                    return FormatDocumentResponse.Error(request.FilePath, message)
        }

    [<JsonRpcMethod(Methods.FormatSelection, UseSingleObjectParameterDeserialization = true)>]
    member _.FormatSelectionAsync(request: FormatSelectionRequest) : Task<FormatSelectionResponse> =
        task {
            let config =
                match request.Config with
                | Some configProperties ->
                    let config = environment.ReadConfiguration request.FilePath
                    parseOptionsFromEditorConfig config configProperties
                | None -> environment.ReadConfiguration request.FilePath

            let selection =
                let r = request.Range

                Range.mkRange
                    request.FilePath
                    (Position.mkPos r.StartLine r.StartColumn)
                    (Position.mkPos r.EndLine r.EndColumn)

            try
                let! formatted, actualSelection =
                    CodeFormatter.FormatSelectionAsync(request.IsSignatureFile, request.SourceCode, selection, config)

                let actualSelection =
                    FormatSelectionRange(
                        actualSelection.StartLine,
                        actualSelection.StartColumn,
                        actualSelection.EndLine,
                        actualSelection.EndColumn
                    )

                return FormatSelectionResponse.Formatted(request.FilePath, formatted, actualSelection)
            with ex ->
                let message =
                    Diagnostics.describeParseFailure request.FilePath (fun () -> request.SourceCode) ex
                    |> Option.defaultValue ex.Message

                return FormatSelectionResponse.Error(request.FilePath, message)
        }

    [<JsonRpcMethod(Methods.Configuration)>]
    member _.Configuration() : string =
        let jsonString (value: string) : JsonNode = JsonValue.Create value :> JsonNode

        let jsonObject (properties: (string * JsonNode) list) : JsonNode =
            let node = JsonObject()

            for key, value in properties do
                node.Add(key, value)

            node :> JsonNode

        let jsonStringArray (values: string list) : JsonNode =
            JsonArray(values |> List.map jsonString |> List.toArray) :> JsonNode

        let settings =
            Reflection.getRecordFields FormatConfig.Default
            |> Array.toList
            |> List.choose (fun (recordField, defaultValue) ->
                let optionalField key value =
                    value |> Option.toList |> List.map (fun v -> key, v)

                let meta =
                    List.concat
                        [| optionalField "category" recordField.Category
                           optionalField "displayName" recordField.DisplayName
                           optionalField "description" recordField.Description |]

                let type' =
                    match defaultValue with
                    | :? bool as b -> Some("boolean", (if b then "true" else "false"))
                    | :? int as i -> Some("number", string<int> i)
                    | :? MultilineFormatterType as m ->
                        Some("multilineFormatterType", MultilineFormatterType.ToConfigString m)
                    | :? EndOfLineStyle as e -> Some("endOfLineStyle", EndOfLineStyle.ToConfigString e)
                    | :? MultilineBracketStyle as m ->
                        Some("multilineBracketStyle", MultilineBracketStyle.ToConfigString m)
                    | _ -> None

                type'
                |> Option.map (fun (typeName, defaultString) ->
                    let value =
                        ("type", typeName) :: ("defaultValue", defaultString) :: meta
                        |> List.map (fun (key, value) -> key, jsonString value)
                        |> jsonObject

                    toEditorConfigName recordField.PropertyName, value))
            |> jsonObject

        let enumOptions =
            jsonObject
                [ "multilineFormatterType",
                  jsonStringArray
                      [ MultilineFormatterType.ToConfigString MultilineFormatterType.CharacterWidth
                        MultilineFormatterType.ToConfigString MultilineFormatterType.NumberOfItems ]
                  "endOfLineStyle",
                  jsonStringArray
                      [ EndOfLineStyle.ToConfigString EndOfLineStyle.LF
                        EndOfLineStyle.ToConfigString EndOfLineStyle.CRLF ]
                  "multilineBracketStyle",
                  jsonStringArray
                      [ MultilineBracketStyle.ToConfigString Aligned
                        MultilineBracketStyle.ToConfigString Cramped
                        MultilineBracketStyle.ToConfigString Stroustrup ] ]

        let json = jsonObject [ "settings", settings; "enumOptions", enumOptions ]

        json.ToJsonString(JsonSerializerOptions(WriteIndented = true, IndentSize = 4))
