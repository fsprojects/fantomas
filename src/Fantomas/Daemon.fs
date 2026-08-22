module Fantomas.Daemon

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.IO.Abstractions
open System.Threading
open System.Threading.Tasks
open StreamJsonRpc
open Thoth.Json.Net
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

/// Resolve the configuration for a request the way the daemon needs it: the `.editorconfig` on
/// disk, then whatever the editor sent layered on top, keeping the problems from both.
///
/// Deliberately silent: the problems travel to the client as a notification and are never written
/// to standard error. `Fantomas.Client` drains the daemon's standard error, but an older client
/// does not, and anything written there then accumulates in a pipe nobody reads. A report is
/// around 1.5KB and the default pipe buffer on Windows is 4KB, so a few of them would block the
/// write inside a format request and hang the connection.
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

    config,
    { FilePath = filePath
      EditorConfigFiles = Array.ofList editorConfigFiles
      Problems = List.toArray (fromEditorConfig @ fromRequest) }

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
                do!
                    notifyConfigurationWarning
                        { FilePath = request.FilePath
                          EditorConfigFiles = Array.empty
                          Problems = Array.empty }

                return FormatDocumentResponse.IgnoredFile request.FilePath
            else
                let cursor =
                    request.Cursor
                    |> Option.map (fun cursor -> CodeFormatter.MakePosition(cursor.Line, cursor.Column))

                try
                    // Reading the configuration belongs inside this: an `.editorconfig` can carry a
                    // value that raises rather than becoming a problem, `end_of_line = cr` among
                    // them, and the client should hear about that as an Error response like any
                    // other failure, not as a remote invocation exception.
                    let config, warning =
                        configurationFor environment.ReadConfiguration request.FilePath request.Config

                    do! notifyConfigurationWarning warning

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
            let selection =
                let r = request.Range

                Range.mkRange
                    request.FilePath
                    (Position.mkPos r.StartLine r.StartColumn)
                    (Position.mkPos r.EndLine r.EndColumn)

            try
                // Inside the try for the same reason as FormatDocumentAsync: a configuration value
                // that raises has to reach the client as an Error response.
                let config, warning =
                    configurationFor environment.ReadConfiguration request.FilePath request.Config

                do! notifyConfigurationWarning warning

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
        let settings =
            Reflection.getRecordFields FormatConfig.Default
            |> Array.toList
            |> List.choose (fun (recordField, defaultValue) ->
                let optionalField key value =
                    value |> Option.toList |> List.map (fun v -> key, Encode.string v)

                let meta =
                    List.concat
                        [| optionalField "category" recordField.Category
                           optionalField "displayName" recordField.DisplayName
                           optionalField "description" recordField.Description |]

                let type' =
                    match defaultValue with
                    | :? bool as b ->
                        Some(
                            Encode.object
                                [ yield "type", Encode.string "boolean"
                                  yield "defaultValue", Encode.string (if b then "true" else "false")
                                  yield! meta ]
                        )
                    | :? int as i ->
                        Some(
                            Encode.object
                                [ yield "type", Encode.string "number"
                                  yield "defaultValue", Encode.string (string<int> i)
                                  yield! meta ]
                        )
                    | :? MultilineFormatterType as m ->
                        Some(
                            Encode.object
                                [ yield "type", Encode.string "multilineFormatterType"
                                  yield "defaultValue", Encode.string (MultilineFormatterType.ToConfigString m)
                                  yield! meta ]
                        )
                    | :? EndOfLineStyle as e ->
                        Some(
                            Encode.object
                                [ yield "type", Encode.string "endOfLineStyle"
                                  yield "defaultValue", Encode.string (EndOfLineStyle.ToConfigString e)
                                  yield! meta ]
                        )
                    | :? MultilineBracketStyle as m ->
                        Some(
                            Encode.object
                                [ yield "type", Encode.string "multilineBracketStyle"
                                  yield "defaultValue", Encode.string (MultilineBracketStyle.ToConfigString m)
                                  yield! meta ]
                        )
                    | _ -> None

                type' |> Option.map (fun t -> toEditorConfigName recordField.PropertyName, t))
            |> Encode.object

        let enumOptions =
            Encode.object
                [ "multilineFormatterType",
                  Encode.list
                      [ (MultilineFormatterType.ToConfigString MultilineFormatterType.CharacterWidth
                         |> Encode.string)
                        (MultilineFormatterType.ToConfigString MultilineFormatterType.NumberOfItems
                         |> Encode.string) ]
                  "endOfLineStyle",
                  Encode.list
                      [ (EndOfLineStyle.ToConfigString EndOfLineStyle.LF |> Encode.string)
                        (EndOfLineStyle.ToConfigString EndOfLineStyle.CRLF |> Encode.string) ]
                  "multilineBracketStyle",
                  Encode.list
                      [ (MultilineBracketStyle.ToConfigString Aligned |> Encode.string)
                        (MultilineBracketStyle.ToConfigString Cramped |> Encode.string)
                        (MultilineBracketStyle.ToConfigString Stroustrup |> Encode.string) ] ]

        Encode.object [ "settings", settings; "enumOptions", enumOptions ]
        |> Encode.toString 4
