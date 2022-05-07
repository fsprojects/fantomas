module Fantomas.Daemon

open System
open System.Diagnostics
open System.IO
open System.IO.Abstractions
open System.Threading
open System.Threading.Tasks
open StreamJsonRpc
open Thoth.Json.Net
open Fantomas.Client.Contracts
open Fantomas.Client.LSPFantomasServiceTypes
open Fantomas.Core
open Fantomas.Core.FormatConfig
open Fantomas.Extras.EditorConfig
open Fantomas.Extras

type FantomasDaemon(sender: Stream, reader: Stream) as this =
    let rpc: JsonRpc = JsonRpc.Attach(sender, reader, this)

    do
        // hook up request/response logging for debugging
        rpc.TraceSource <- TraceSource(typeof<FantomasDaemon>.Name, SourceLevels.Verbose)

        rpc.TraceSource.Listeners.Add(new SerilogTraceListener.SerilogTraceListener(typeof<FantomasDaemon>.Name))
        |> ignore<int>

    let disconnectEvent = new ManualResetEvent(false)

    let exit () = disconnectEvent.Set() |> ignore

    let fs = FileSystem()

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
            if IgnoreFile.isIgnoredFile (IgnoreFile.find fs IgnoreFile.loadIgnoreList request.FilePath) request.FilePath then
                return FormatDocumentResponse.IgnoredFile request.FilePath
            else
                let config =
                    match request.Config with
                    | Some configProperties ->
                        let config = readConfiguration request.FilePath
                        parseOptionsFromEditorConfig config configProperties
                    | None -> readConfiguration request.FilePath

                try
                    let! formatted =
                        let isSignature = request.FilePath.EndsWith(".fsi")
                        CodeFormatter.FormatDocumentAsync(isSignature, request.SourceCode, config)

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
            return
                FormatSelectionResponse.Error(
                    request.FilePath,
                    "Format selection is no longer supported in Fantomas 5."
                )
        //            let config =
        //                match request.Config with
        //                | Some configProperties ->
        //                    let config = readConfiguration request.FilePath
        //                    parseOptionsFromEditorConfig config configProperties
        //                | None -> readConfiguration request.FilePath
        //
        //            let range =
        //                let r = request.Range
        //                mkRange request.FilePath (mkPos r.StartLine r.StartColumn) (mkPos r.EndLine r.EndColumn)
        //
        //            try
        //                let! formatted =
        //                    CodeFormatter.FormatSelectionAsync(
        //                        request.FilePath,
        //                        range,
        //                        SourceString request.SourceCode,
        //                        config,
        //                        CodeFormatterImpl.createParsingOptionsFromFile request.FilePath,
        //                        CodeFormatterImpl.sharedChecker.Value
        //                    )
        //
        //                return FormatSelectionResponse.Formatted(request.FilePath, formatted)
        //            with
        //            | ex -> return FormatSelectionResponse.Error(request.FilePath, ex.Message)
        }
        |> Async.StartAsTask

    [<JsonRpcMethod(Methods.Configuration)>]
    member _.Configuration() : string =
        let settings =
            Reflection.getRecordFields FormatConfig.FormatConfig.Default
            |> Array.toList
            |> List.choose (fun (recordField, defaultValue) ->
                let optionalField key value =
                    value
                    |> Option.toList
                    |> List.map (fun v -> key, Encode.string v)

                let meta =
                    List.concat [| optionalField "category" recordField.Category
                                   optionalField "displayName" recordField.DisplayName
                                   optionalField "description" recordField.Description |]

                let type' =
                    match defaultValue with
                    | :? bool as b ->
                        Some(
                            Encode.object [ yield "type", Encode.string "boolean"
                                            yield "defaultValue", Encode.string (if b then "true" else "false")
                                            yield! meta ]
                        )
                    | :? int as i ->
                        Some(
                            Encode.object [ yield "type", Encode.string "number"
                                            yield "defaultValue", Encode.string (string i)
                                            yield! meta ]
                        )
                    | :? MultilineFormatterType as m ->
                        Some(
                            Encode.object [ yield "type", Encode.string "multilineFormatterType"
                                            yield
                                                "defaultValue", Encode.string (MultilineFormatterType.ToConfigString m)
                                            yield! meta ]
                        )
                    | :? EndOfLineStyle as e ->
                        Some(
                            Encode.object [ yield "type", Encode.string "endOfLineStyle"
                                            yield "defaultValue", Encode.string (EndOfLineStyle.ToConfigString e)
                                            yield! meta ]
                        )
                    | _ -> None

                type'
                |> Option.map (fun t -> toEditorConfigName recordField.PropertyName, t))
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
