module Fantomas.Tests.Integration.DaemonWireTests

open System.IO
open System.Text.Json
open System.Collections.Generic
open System.Threading.Tasks
open Newtonsoft.Json.Linq
open NUnit.Framework
open StreamJsonRpc
open Fantomas.Client.Contracts
open Fantomas.Tests

// What goes over the wire between `Fantomas.Client` and a daemon, message by message.
//
// `DaemonTests` checks that a client and a daemon understand each other. That is not enough to
// change how the daemon serializes: an editor ships its own `Fantomas.Client`, often an older one,
// and talks to whichever Fantomas the user installed, so the daemon has to keep sending what those
// clients were written against. These tests pin that down.
//
// Every `.json` file in `DaemonWire` is a case, named after what it shows, with its snapshot beside it:
//
// - `<case>.json` describes it: a `description`, the `files` to put in the folder the daemon runs in,
//   and the `requests` to send, in order. A request has a `method` and, for the format methods,
//   `params` named after the fields of `FormatDocumentRequest` and `FormatSelectionRequest`, with
//   `FilePath` relative to that folder and an option written as its value or left out. The request
//   is built from those as the client's own type and sent the way `Fantomas.Client` sends it.
// - `<case>.gold` is every message that went over the wire: `sent` is what the client wrote,
//   `received` what the daemon answered, with the folder and the version as placeholders.
//
// A case whose conversation differs fails with a line diff and leaves `<case>.actual` beside
// `<case>.gold`. Rename it over the gold to accept it, or run the `UpdateSnapshots` pipeline of
// `build.fsx` to accept every change. `FANTOMAS_EXECUTABLE` points the cases at another build
// of the tool, a Native AOT one for instance.

let private casesFolder: string = Path.Join(__SOURCE_DIRECTORY__, "DaemonWire")

/// Send one request from an `input.json`, the way `Fantomas.Client` sends it, and wait for the answer.
///
/// An error response is part of the conversation like any other, so it is recorded rather than
/// failing the case.
let private send (client: JsonRpc) (folder: string) (request: JsonElement) : Task =
    task {
        let methodName: string = request.GetProperty("method").GetString()

        let parameters: JsonElement option =
            match request.TryGetProperty "params" with
            | true, parameters -> Some parameters
            | false, _ -> None

        let property (name: string) : JsonElement option =
            parameters
            |> Option.bind (fun parameters ->
                match parameters.TryGetProperty name with
                | true, value when value.ValueKind <> JsonValueKind.Null -> Some value
                | _ -> None
            )

        let text (name: string) : string =
            property name |> Option.map (fun value -> value.GetString()) |> Option.toObj

        let number (value: JsonElement) (name: string) : int = value.GetProperty(name).GetInt32()

        let filePath () : string = Path.Join(folder, text "FilePath")

        let config () : IReadOnlyDictionary<string, string> option =
            property "Config"
            |> Option.map (fun config ->
                config.EnumerateObject()
                |> Seq.map (fun setting -> setting.Name, setting.Value.GetString())
                |> readOnlyDict
            )

        let argument: obj option =
            match methodName with
            | Methods.FormatDocument ->
                let request: FormatDocumentRequest =
                    {
                        SourceCode = text "SourceCode"
                        FilePath = filePath ()
                        Config = config ()
                        Cursor =
                            property "Cursor"
                            |> Option.map (fun cursor ->
                                FormatCursorPosition(number cursor "Line", number cursor "Column")
                            )
                    }

                Some(box request)
            | Methods.FormatSelection ->
                let range: JsonElement =
                    match property "Range" with
                    | Some range -> range
                    | None -> failwith $"A %s{Methods.FormatSelection} request in a case needs a Range."

                let request: FormatSelectionRequest =
                    {
                        SourceCode = text "SourceCode"
                        FilePath = filePath ()
                        Config = config ()
                        Range =
                            FormatSelectionRange(
                                number range "StartLine",
                                number range "StartColumn",
                                number range "EndLine",
                                number range "EndColumn"
                            )
                    }

                Some(box request)
            | _ ->
                parameters
                |> Option.map (fun parameters -> box (JToken.Parse(parameters.GetRawText())))

        try
            match argument with
            | Some argument ->
                let! _ = client.InvokeWithParameterObjectAsync<JToken>(methodName, argument)
                ()
            | None ->
                let! _ = client.InvokeAsync<JToken>(methodName)
                ()
        with :? RemoteInvocationException ->
            ()
    }

let cases () : string array =
    Directory.GetFiles(casesFolder, "*.json")
    |> Array.map Path.GetFileNameWithoutExtension
    |> Array.sort

[<Test>]
[<Category("Snapshot")>]
[<TestCaseSource(nameof cases)>]
let conversation (case: string) : unit =
    use input: JsonDocument =
        JsonDocument.Parse(File.ReadAllText(Path.Join(casesFolder, $"%s{case}.json")))

    let files: (string * string) list =
        match input.RootElement.TryGetProperty "files" with
        | true, files -> [ for file in files.EnumerateObject() -> file.Name, file.Value.GetString() ]
        | false, _ -> []

    let requests: JsonElement list =
        [
            for request in input.RootElement.GetProperty("requests").EnumerateArray() -> request
        ]

    let recorded: string =
        DaemonConversation.record
            files
            (fun client workspace ->
                task {
                    for request in requests do
                        do! send client workspace request
                }
            )

    Snapshot.verify (Path.Join(casesFolder, $"%s{case}.gold")) (recorded + "\n")
