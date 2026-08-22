module Fantomas.Daemon

open System
open System.Collections.Generic
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
      ReadConfiguration: string -> EditorConfigResult option
      Log: ILogger }

let toConfigurationProblem (source: ConfigurationProblemSource) (problem: EditorConfigProblem) : ConfigurationProblem =
    match problem with
    | EditorConfigProblem.UnknownSetting setting ->
        { Code = int ConfigurationProblemCode.UnknownSetting
          Source = int source
          Setting = setting
          Value = null }
    | EditorConfigProblem.UnrecognizedValue(setting, value) ->
        { Code = int ConfigurationProblemCode.UnrecognizedValue
          Source = int source
          Setting = setting
          Value = value }

let configurationFor
    (readConfiguration: string -> EditorConfigResult option)
    (filePath: string)
    (requestConfig: IReadOnlyDictionary<string, string> option)
    : FormatConfig * ConfigurationWarning =
    let config, editorConfigFiles, fileProblems =
        match readConfiguration filePath with
        | Some result -> result.Config, result.EditorConfigFiles, result.Problems
        | None -> FormatConfig.Default, [], []

    let fromEditorConfig =
        List.map (toConfigurationProblem ConfigurationProblemSource.EditorConfig) fileProblems

    let config, fromRequest =
        match requestConfig with
        | None -> config, []
        | Some properties ->
            let config, problems = parseOptionsFromEditorConfig config properties
            config, List.map (toConfigurationProblem ConfigurationProblemSource.Request) problems

    let problems = List.toArray (fromEditorConfig @ fromRequest)

    config,
    { FilePath = filePath
      // Only worth sending when there is something to point at. It is the same list on every
      // request for a file, and a client has nothing to do with it while nothing is wrong.
      EditorConfigFiles =
        if Array.isEmpty problems then
            Array.empty
        else
            Array.ofList editorConfigFiles
      Problems = problems }

let noConfigurationProblems (filePath: string) : ConfigurationWarning =
    { FilePath = filePath
      EditorConfigFiles = Array.empty
      Problems = Array.empty }

type FantomasDaemon(sender: Stream, reader: Stream, environment: DaemonEnvironment) as this =
    let rpc: JsonRpc = JsonRpc.Attach(sender, reader, this)
    let traceListener = new DefaultTraceListener()

    do
        // hook up request/response logging for debugging
        rpc.TraceSource <- TraceSource(typeof<FantomasDaemon>.Name, SourceLevels.Verbose)
        rpc.TraceSource.Listeners.Add traceListener |> ignore<int>

    /// Tell the client which settings in the resolved configuration could not be acted on. Sent
    /// after every request, with an empty list when nothing is wrong, so a client can clear what
    /// it showed earlier. A client that does not handle the method ignores it.
    let notifyConfigurationWarning (warning: ConfigurationWarning) : Task =
        task {
            try
                do! rpc.NotifyAsync(Methods.ConfigurationWarning, [| box warning |])
            with _ ->
                // The client went away, or does not speak this method. Never fail a format
                // request over a message that only carries advice.
                ()
        }

    /// Resolve the configuration for a request, report it to the client, and hand it to `format`.
    ///
    /// Owns the whole ordering contract in one place, for both format methods: the notification is
    /// started before formatting, so the write is not on the way to a formatted document, and it is
    /// awaited before returning, so a client never sees the answer to a request before the warning
    /// that belongs to it. That has to hold on the way out through the error path too, which is why
    /// the started task is awaited in the `with` as well as the happy path.
    ///
    /// Reading the configuration can raise rather than come back with a problem, `end_of_line = cr`
    /// being the one that does. Then nothing has been sent, so an empty warning goes out instead,
    /// rather than leaving the client showing what the previous request left it with.
    let withConfiguration
        (filePath: string)
        (sourceCode: string)
        (requestConfig: IReadOnlyDictionary<string, string> option)
        (format: FormatConfig -> Task<'response>)
        (onError: string -> 'response)
        : Task<'response> =
        task {
            let mutable notified: Task = null

            try
                let config, warning =
                    configurationFor environment.ReadConfiguration filePath requestConfig

                notified <- notifyConfigurationWarning warning
                let! response = format config
                do! notified
                return response
            with ex ->
                if isNull notified then
                    do! notifyConfigurationWarning (noConfigurationProblems filePath)
                else
                    do! notified

                // A ParseException's own Message is an %A dump of the diagnostic records, and it is
                // the editor's user who would have been shown it.
                let message =
                    Diagnostics.describeParseFailure filePath (fun () -> sourceCode) ex
                    |> Option.defaultValue ex.Message

                return onError message
        }

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
                // Still reported, with nothing in it, so a client can clear what an earlier request
                // showed for this file rather than leaving a stale warning behind.
                do! notifyConfigurationWarning (noConfigurationProblems request.FilePath)

                return FormatDocumentResponse.IgnoredFile request.FilePath
            else
                let cursor =
                    request.Cursor
                    |> Option.map (fun cursor -> CodeFormatter.MakePosition(cursor.Line, cursor.Column))

                return!
                    withConfiguration
                        request.FilePath
                        request.SourceCode
                        request.Config
                        (fun config ->
                            task {
                                let! formatResponse =
                                    match cursor with
                                    | None ->
                                        CodeFormatter.FormatDocumentAsync(
                                            request.IsSignatureFile,
                                            request.SourceCode,
                                            config
                                        )
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
                                        |> Option.map (fun cursorPos ->
                                            FormatCursorPosition(cursorPos.Line, cursorPos.Column))

                                    return
                                        FormatDocumentResponse.Formatted(request.FilePath, formatResponse.Code, cursor)
                            })
                        (fun message -> FormatDocumentResponse.Error(request.FilePath, message))
        }

    [<JsonRpcMethod(Methods.FormatSelection, UseSingleObjectParameterDeserialization = true)>]
    member _.FormatSelectionAsync(request: FormatSelectionRequest) : Task<FormatSelectionResponse> =
        let selection =
            let r = request.Range

            Range.mkRange
                request.FilePath
                (Position.mkPos r.StartLine r.StartColumn)
                (Position.mkPos r.EndLine r.EndColumn)

        withConfiguration
            request.FilePath
            request.SourceCode
            request.Config
            (fun config ->
                task {
                    let! formatted, actualSelection =
                        CodeFormatter.FormatSelectionAsync(
                            request.IsSignatureFile,
                            request.SourceCode,
                            selection,
                            config
                        )

                    let actualSelection =
                        FormatSelectionRange(
                            actualSelection.StartLine,
                            actualSelection.StartColumn,
                            actualSelection.EndLine,
                            actualSelection.EndColumn
                        )

                    return FormatSelectionResponse.Formatted(request.FilePath, formatted, actualSelection)
                })
            (fun message -> FormatSelectionResponse.Error(request.FilePath, message))

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
