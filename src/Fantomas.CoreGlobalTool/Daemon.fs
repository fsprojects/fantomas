module Fantomas.CoreGlobalTool.Daemon

open System
open System.Diagnostics
open System.IO
open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Text.Range
open FSharp.Compiler.Text.Pos
open Fantomas.Client.Contracts
open StreamJsonRpc
open Fantomas
open Fantomas.SourceOrigin
open Fantomas.FormatConfig
open Fantomas.Extras.EditorConfig

let private createParsingOptionsFromFile (isLastFile: bool) (fileName: string) : FSharpParsingOptions =
    let additionFile =
        if not isLastFile then
            let name = Guid.NewGuid().ToString("N")
            [ $"{name}.fs" ]
        else
            List.empty

    { FSharpParsingOptions.Default with
          SourceFiles = [| fileName; yield! additionFile |] }

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
                            createParsingOptionsFromFile request.IsLastFile request.FilePath,
                            CodeFormatterImpl.sharedChecker.Value
                        )

                    if formatted = request.SourceCode then
                        return FormatDocumentResponse.Unchanged request.FilePath
                    else
                        return FormatDocumentResponse.Formatted(request.FilePath, formatted)
                with
                | ex -> return FormatDocumentResponse.Error(request.FilePath, ex)
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

            let! formatted =
                CodeFormatter.FormatSelectionAsync(
                    request.FilePath, // TODO: does this really work with FSI??
                    range,
                    SourceString request.SourceCode,
                    config,
                    CodeFormatterImpl.createParsingOptionsFromFile request.FilePath, // Use safe name ??
                    CodeFormatterImpl.sharedChecker.Value
                )

            return FormatSelectionResponse.Formatted(request.FilePath, formatted)
        }
        |> Async.StartAsTask

    [<JsonRpcMethod(Methods.Configuration)>]
    member _.Configuration() : ConfigurationResponse =
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
