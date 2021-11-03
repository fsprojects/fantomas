module Fantomas.CoreGlobalTool.Daemon

open System
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Text.Position
open StreamJsonRpc
open Thoth.Json.Net
open Fantomas.Client.Contracts
open Fantomas.Client.LSPFantomasServiceTypes
open Fantomas
open Fantomas.SourceOrigin
open Fantomas.FormatConfig
open Fantomas.Extras.EditorConfig

type FantomasDaemon(sender: Stream, reader: Stream) as this =
    let rpc: JsonRpc = JsonRpc.Attach(sender, reader, this)

    do
        // hook up request/response logging for debugging
        rpc.TraceSource <- TraceSource(typeof<FantomasDaemon>.Name, SourceLevels.Verbose)

        rpc.TraceSource.Listeners.Add(new SerilogTraceListener.SerilogTraceListener(typeof<FantomasDaemon>.Name))
        |> ignore<int>

    let disconnectEvent = new ManualResetEvent(false)

    let exit () = disconnectEvent.Set() |> ignore

    do rpc.Disconnected.Add(fun _ -> exit ())

    interface IDisposable with
        member this.Dispose() = disconnectEvent.Dispose()

    /// returns a hot task that resolves when the stream has terminated
    member this.WaitForClose = rpc.Completion

    [<JsonRpcMethod(Methods.Version)>]
    member _.Version() : string = CodeFormatter.GetVersion()

    [<JsonRpcMethod(Methods.FormatDocument, UseSingleObjectParameterDeserialization = true)>]
    member _.FormatDocumentAsync(request: FormatDocumentRequest) : Task<FormatDocumentResponse> =
        async {
            if Fantomas.Extras.IgnoreFile.isIgnoredFile request.FilePath then
                return FormatDocumentResponse.IgnoredFile request.FilePath
            else
                let config =
                    match request.Config with
                    | Some configProperties -> parseOptionsFromEditorConfig configProperties
                    | None -> readConfiguration request.FilePath

                try
                    let! formatted =
                        CodeFormatter.FormatDocumentAsync(
                            request.FilePath,
                            SourceString request.SourceCode,
                            config,
                            CodeFormatterImpl.createParsingOptionsFromFile request.FilePath,
                            CodeFormatterImpl.sharedChecker.Value
                        )

                    if formatted = request.SourceCode then
                        return FormatDocumentResponse.Unchanged request.FilePath
                    else
                        return FormatDocumentResponse.Formatted(request.FilePath, formatted)
                with
                | ex -> return FormatDocumentResponse.Error(request.FilePath, ex.Message)
        }
        |> Async.StartAsTask

    [<JsonRpcMethod(Methods.FormatSelection, UseSingleObjectParameterDeserialization = true)>]
    member _.FormatSelectionAsync(request: FormatSelectionRequest) : Task<FormatSelectionResponse> =
        async {
            let config =
                match request.Config with
                | Some configProperties -> parseOptionsFromEditorConfig configProperties
                | None -> readConfiguration request.FilePath

            let range =
                let r = request.Range
                mkRange request.FilePath (mkPos r.StartLine r.StartColumn) (mkPos r.EndLine r.EndColumn)

            try
                let! formatted =
                    CodeFormatter.FormatSelectionAsync(
                        request.FilePath,
                        range,
                        SourceString request.SourceCode,
                        config,
                        CodeFormatterImpl.createParsingOptionsFromFile request.FilePath,
                        CodeFormatterImpl.sharedChecker.Value
                    )

                return FormatSelectionResponse.Formatted(request.FilePath, formatted)
            with
            | ex -> return FormatSelectionResponse.Error(request.FilePath, ex.Message)
        }
        |> Async.StartAsTask

    [<JsonRpcMethod(Methods.Configuration)>]
    member _.Configuration() : string =
        let settings =
            Reflection.getRecordFields FormatConfig.FormatConfig.Default
            |> Array.toList
            |> List.choose
                (fun (name, defaultValue) ->
                    let type' =
                        match defaultValue with
                        | :? bool as b ->
                            Some(
                                Encode.object [ "type", Encode.string "boolean"
                                                "defaultValue", Encode.string (if b then "true" else "false") ]
                            )
                        | :? int as i ->
                            Some(
                                Encode.object [ "type", Encode.string "number"
                                                "defaultValue", Encode.string (string i) ]
                            )
                        | :? MultilineFormatterType as m ->
                            Some(
                                Encode.object [ "type", Encode.string "multilineFormatterType"
                                                "defaultValue", Encode.string (MultilineFormatterType.ToConfigString m) ]
                            )
                        | :? EndOfLineStyle as e ->
                            Some(
                                Encode.object [ "type", Encode.string "endOfLineStyle"
                                                "defaultValue", Encode.string (EndOfLineStyle.ToConfigString e) ]
                            )
                        | _ -> None

                    type'
                    |> Option.map (fun t -> toEditorConfigName name, t))
            |> Encode.object

        let enumOptions =
            Encode.object [ "multilineFormatterType",
                            Encode.list [ (MultilineFormatterType.ToConfigString MultilineFormatterType.CharacterWidth
                                           |> Encode.string)
                                          (MultilineFormatterType.ToConfigString MultilineFormatterType.NumberOfItems
                                           |> Encode.string) ]
                            "endOfLineStyle",
                            Encode.list [ (EndOfLineStyle.ToConfigString EndOfLineStyle.LF
                                           |> Encode.string)
                                          (EndOfLineStyle.ToConfigString EndOfLineStyle.CRLF
                                           |> Encode.string) ] ]

        Encode.object [ "settings", settings
                        "enumOptions", enumOptions ]
        |> Encode.toString 4
