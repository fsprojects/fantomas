module Fantomas.CoreGlobalTool.Daemon

open System
open System.Diagnostics
open System.IO
open System.Threading.Tasks
open Fantomas
open Fantomas.SourceOrigin
open StreamJsonRpc
open System.Threading
open Fantomas.FormatConfig
open Fantomas.Extras.EditorConfig
open Fantomas.Client.Contracts

type FantomasDaemon(sender: Stream, reader: Stream) as this =
    let rpc: JsonRpc = JsonRpc.Attach(sender, reader, this)

    do
        // hook up request/response logging for debugging
        rpc.TraceSource <- TraceSource(typeof<FantomasDaemon>.Name, SourceLevels.Verbose)

    //        rpc.TraceSource.Listeners.Add(new SerilogTraceListener.SerilogTraceListener(typeof<FantomasLSPServer>.Name))
//        |> ignore<int>

    let disconnectEvent = new ManualResetEvent(false)

    let exit () = disconnectEvent.Set() |> ignore

    do rpc.Disconnected.Add(fun _ -> exit ())

    interface IDisposable with
        member this.Dispose() = disconnectEvent.Dispose()

    /// returns a hot task that resolves when the stream has terminated
    member this.WaitForClose = rpc.Completion

    [<JsonRpcMethod(Methods.Version)>]
    member _.Version() : VersionResponse =
        { Version = CodeFormatter.GetVersion() }

    [<JsonRpcMethod(Methods.FormatDocument, UseSingleObjectParameterDeserialization = true)>]
    member _.FormatDocumentAsync(options: FormatDocumentOptions) : Task<FormatDocumentResponse> =
        async {
            let! formatted =
                CodeFormatter.FormatDocumentAsync(
                    options.FilePath,
                    SourceString options.SourceCode,
                    FormatConfig.FormatConfig.Default,
                    CodeFormatterImpl.createParsingOptionsFromFile options.FilePath,
                    CodeFormatterImpl.sharedChecker.Value
                )

            return { Formatted = formatted }
        }
        |> Async.StartAsTask

    [<JsonRpcMethod(Methods.Configuration)>]
    member _.Configuration() : ConfigurationResult =
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
