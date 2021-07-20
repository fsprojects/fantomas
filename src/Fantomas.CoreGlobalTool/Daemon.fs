module Fantomas.CoreGlobalTool.Daemon

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open Fantomas
open Fantomas.SourceOrigin
open LspTypes
open StreamJsonRpc
open System.Threading
open Fantomas.FormatConfig
open Fantomas.Extras.EditorConfig

type FormatDocumentOptions =
    { SourceCode: string
      /// File path will be used to identify the .editorconfig options
      /// Unless the configuration is passed
      FilePath: string
      /// Overrides the found .editorconfig.
      Config: string option }

type FormatSourceRange =
    class
    end

type FantomasOption = { Type: string; DefaultValue: string }

type ConfigurationResult =
    { Options: IReadOnlyDictionary<string, FantomasOption>
      EnumOptions: IReadOnlyDictionary<string, string array> }

type VersionResult = { Version: string }

type FormatResponse = { Formatted: string }

type FantomasLSPServer(sender: Stream, reader: Stream) as this =
    let rpc: JsonRpc = JsonRpc.Attach(sender, reader, this)

    do
        // hook up request/response logging for debugging
        rpc.TraceSource <- TraceSource(typeof<FantomasLSPServer>.Name, SourceLevels.Verbose)

    //        rpc.TraceSource.Listeners.Add(new SerilogTraceListener.SerilogTraceListener(typeof<FantomasLSPServer>.Name))
//        |> ignore<int>

    let disconnectEvent = new ManualResetEvent(false)

    let exit () = disconnectEvent.Set() |> ignore

    do rpc.Disconnected.Add(fun _ -> exit ())

    interface IDisposable with
        member this.Dispose() = disconnectEvent.Dispose()

    /// returns a hot task that resolves when the stream has terminated
    member this.WaitForClose = rpc.Completion

    /// Fantomas uses the LSP protocol but does initially not aim to function as fully fledged LSP server.
    /// Custom RPC methods are introduced to take no dependency on the file system and not required the constant communication of file events.
    [<JsonRpcMethod("fantomas/formatDocument", UseSingleObjectParameterDeserialization = true)>]
    member this.FormatSource(options: FormatDocumentOptions) : Async<FormatResponse> = // TODO: later Task
        //        let filePath =
//            Path.FileUriToLocalPath options.TextDocument.Uri
//
//        let response : TextEdit = TextEdit()
//        let range = Range()
//        range.Start <- (Position(0u, 0u))
//        range.End <- (Position(countLines options.SourceCode - 1u, 0u))
//        response.Range <- range

        let config = FormatConfig.Default
        //            match Option.ofObj options.Config with
//            | Some options -> parseOptionsFromEditorConfig options
//            | None -> readConfiguration filePath


        async {
            let! formatted =
                CodeFormatter.FormatDocumentAsync(
                    options.FilePath,
                    SourceString options.SourceCode,
                    config,
                    CodeFormatterImpl.createParsingOptionsFromFile options.FilePath,
                    CodeFormatterImpl.sharedChecker.Value
                )

            return { Formatted = formatted }
        }


    [<JsonRpcMethod("fantomas/formatSelection")>]
    member this.FormatSourceRange(options: FormatDocumentOptions) : TextEdit = failwith "not yet implemented"

    [<JsonRpcMethod("fantomas/configuration")>]
    member this.Configuration() : ConfigurationResult =
        let options =
            Reflection.getRecordFields FormatConfig.FormatConfig.Default
            |> Array.choose
                (fun (name, defaultValue) ->
                    let type' =
                        match defaultValue with
                        | :? bool as b ->
                            Some
                                { Type = "boolean"
                                  DefaultValue = if b then "true" else "false" }
                        | :? int as i ->
                            Some
                                { Type = "number"
                                  DefaultValue = string i }
                        | :? MultilineFormatterType as m ->
                            Some
                                { Type = "multilineFormatterType"
                                  DefaultValue = MultilineFormatterType.ToConfigString m }
                        | :? EndOfLineStyle as e ->
                            Some
                                { Type = "endOfLineStyle"
                                  DefaultValue = EndOfLineStyle.ToConfigString e }
                        | _ -> None

                    type'
                    |> Option.map (fun t -> toEditorConfigName name, t))
            |> readOnlyDict

        let enumOptions =
            [ "multilineFormatterType",
              [| MultilineFormatterType.ToConfigString MultilineFormatterType.CharacterWidth
                 MultilineFormatterType.ToConfigString MultilineFormatterType.NumberOfItems |]
              "endOfLineStyle",
              [| EndOfLineStyle.ToConfigString EndOfLineStyle.LF
                 EndOfLineStyle.ToConfigString EndOfLineStyle.CRLF |] ]
            |> readOnlyDict

        { Options = options
          EnumOptions = enumOptions }

    [<JsonRpcMethod("fantomas/version")>]
    member this.Version() : VersionResult =
        { Version = CodeFormatter.GetVersion() }
