module Fantomas.Tests.Integration.DaemonConversation

open System
open System.Diagnostics
open System.IO
open System.Text
open System.Text.Encodings.Web
open System.Text.Json
open System.Text.Json.Nodes
open System.Threading
open System.Threading.Tasks
open NUnit.Framework
open StreamJsonRpc
open Fantomas.Core
open Fantomas.Tests.TestHelpers

// A conversation with a real daemon process, recorded byte for byte on the client's side and turned
// into JSON that can be compared with a snapshot.
//
// `FANTOMAS_DAEMON_EXECUTABLE` starts another build of the tool instead of the one this build put
// next to the tests, a Native AOT one for instance.

/// A stream that keeps a copy of every byte read from or written to it.
type private RecordingStream(inner: Stream) =
    inherit Stream()

    let recorded: MemoryStream = new MemoryStream()

    let record (bytes: ReadOnlySpan<byte>) : unit =
        let copy: byte array = bytes.ToArray()
        lock recorded (fun () -> recorded.Write(copy, 0, copy.Length))

    member _.Recorded: byte array = lock recorded (fun () -> recorded.ToArray())

    override _.CanRead = inner.CanRead
    override _.CanWrite = inner.CanWrite
    override _.CanSeek = false
    override _.Length = raise (NotSupportedException())

    override _.Position
        with get () = raise (NotSupportedException())
        and set _ = raise (NotSupportedException())

    override _.Seek(_, _) = raise (NotSupportedException())
    override _.SetLength _ = raise (NotSupportedException())
    override _.Flush() = inner.Flush()
    override _.FlushAsync(cancellationToken: CancellationToken) = inner.FlushAsync cancellationToken

    override _.Read(buffer: byte array, offset: int, count: int) : int =
        let read: int = inner.Read(buffer, offset, count)
        record (ReadOnlySpan(buffer, offset, read))
        read

    override _.ReadAsync(buffer: Memory<byte>, cancellationToken: CancellationToken) : ValueTask<int> =
        ValueTask<int>(
            task {
                let! read = inner.ReadAsync(buffer, cancellationToken)
                record (Span<byte>.op_Implicit(buffer.Span.Slice(0, read)))
                return read
            }
        )

    override this.ReadAsync(buffer: byte array, offset: int, count: int, cancellationToken: CancellationToken) =
        this.ReadAsync(Memory(buffer, offset, count), cancellationToken).AsTask()

    override _.Write(buffer: byte array, offset: int, count: int) : unit =
        record (ReadOnlySpan(buffer, offset, count))
        inner.Write(buffer, offset, count)

    override _.WriteAsync(buffer: ReadOnlyMemory<byte>, cancellationToken: CancellationToken) : ValueTask =
        record buffer.Span
        inner.WriteAsync(buffer, cancellationToken)

    override this.WriteAsync(buffer: byte array, offset: int, count: int, cancellationToken: CancellationToken) =
        this.WriteAsync(ReadOnlyMemory(buffer, offset, count), cancellationToken).AsTask()

    override _.Dispose(disposing: bool) =
        if disposing then
            inner.Dispose()
            recorded.Dispose()

/// The bodies of the header delimited messages in a recording, in the order they were sent.
let private messagesIn (recording: byte array) : string list =
    let separator: byte array = "\r\n\r\n"B

    let rec read (offset: int) (messages: string list) : string list =
        if offset >= recording.Length then
            List.rev messages
        else

        let headerEnd: int = recording.AsSpan(offset).IndexOf(ReadOnlySpan separator)

        if headerEnd < 0 then
            failwith $"An incomplete message header at byte %i{offset}"

        let headers: string = Encoding.ASCII.GetString(recording, offset, headerEnd)

        let contentLength: int =
            headers.Split("\r\n")
            |> Array.pick (fun header ->
                let parts: string array = header.Split(':', 2)

                if parts.[0].Trim().Equals("Content-Length", StringComparison.OrdinalIgnoreCase) then
                    Some(int (parts.[1].Trim()))
                else
                    None
            )

        let bodyStart: int = offset + headerEnd + separator.Length
        let body: string = Encoding.UTF8.GetString(recording, bodyStart, contentLength)
        read (bodyStart + contentLength) (body :: messages)

    read 0 []

/// `node` with `replacements` applied to every string in it, however deep.
///
/// Applied to the values rather than to the JSON text, because a JSON writer escapes the backslashes
/// of a Windows path, and the text of a message then no longer contains the folder it names.
let rec private replaceInStrings (replacements: (string * string) list) (node: JsonNode) : JsonNode =
    match node with
    | null -> null
    | :? JsonObject as object ->
        let replaced: JsonObject = JsonObject()

        for property in object do
            replaced.Add(property.Key, replaceInStrings replacements property.Value)

        replaced
    | :? JsonArray as array -> JsonArray(array |> Seq.map (replaceInStrings replacements) |> Array.ofSeq)
    | :? JsonValue as value when value.GetValueKind() = JsonValueKind.String ->
        replacements
        |> List.fold
            (fun (text: string) (original: string, placeholder: string) -> text.Replace(original, placeholder))
            (value.GetValue<string>())
        |> JsonValue.Create
        :> JsonNode
    | _ -> node.DeepClone()

/// A message as it is compared: the envelope's properties in a fixed order, because no client reads
/// them in any particular one, and everything below the envelope in the order it was sent, because
/// Newtonsoft needs a union's `Case` before its `Fields`.
let private canonical (replacements: (string * string) list) (message: string) : JsonNode =
    let envelope: JsonObject = JsonNode.Parse(message).AsObject()
    let sorted: JsonObject = JsonObject()

    for property in envelope |> Seq.sortBy (fun property -> property.Key) do
        sorted.Add(property.Key, replaceInStrings replacements property.Value)

    sorted

let private daemonExecutable () : string =
    let executable: string =
        Environment.GetEnvironmentVariable "FANTOMAS_DAEMON_EXECUTABLE"

    if String.IsNullOrEmpty executable then
        fantomasExecutable ()
    else
        executable

/// Run one conversation against a fresh daemon process in a folder of its own, and return every
/// message that went over the wire as one JSON document: `sent` is what the client wrote, `received`
/// what the daemon answered, with the folder and the version replaced by placeholders.
///
/// `files` are written into that folder first. `conversation` is handed a client connected to the
/// daemon and the folder, and the recording ends when it completes.
let record (files: (string * string) list) (conversation: JsonRpc -> string -> Task<unit>) : string =
    let folder: string = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory folder |> ignore

    try
        // Every case formats with `\n` line endings, whatever the platform. `end_of_line` otherwise
        // follows the platform, and that decides more than the newlines in a response: formatting
        // `module Foobar\n` on Windows gives `\r\n` and so a changed file where Linux has an
        // unchanged one. A case that brings its own `.editorconfig` gets this appended to it.
        let lineEndings: string = "\n[*]\nend_of_line = lf\n"

        // `root = true` so that no `.editorconfig` above the temp folder on this machine leaks in. A
        // case that brings its own has to say it too.
        File.WriteAllText(Path.Join(folder, ".editorconfig"), "root = true\n" + lineEndings)

        for fileName, content in files do
            let content: string =
                if fileName = ".editorconfig" then
                    content + lineEndings
                else
                    content

            File.WriteAllText(Path.Join(folder, fileName), content)

        let startInfo: ProcessStartInfo =
            ProcessStartInfo(daemonExecutable (), [ "--daemon" ])

        startInfo.UseShellExecute <- false
        startInfo.WorkingDirectory <- folder
        startInfo.RedirectStandardInput <- true
        startInfo.RedirectStandardOutput <- true
        startInfo.RedirectStandardError <- true

        use daemon = Process.Start startInfo
        let standardError: Task<string> = daemon.StandardError.ReadToEndAsync()
        let sent: RecordingStream = new RecordingStream(daemon.StandardInput.BaseStream)

        let received: RecordingStream =
            new RecordingStream(daemon.StandardOutput.BaseStream)

        let client: JsonRpc = new JsonRpc(sent, received)
        client.StartListening()

        try
            let completed: bool = (conversation client folder).Wait(TimeSpan.FromSeconds 30.)

            if not completed then
                Assert.Fail $"The exchange did not complete. Standard error:\n%s{standardError.Result}"
        finally
            client.Dispose()

            if not (daemon.WaitForExit(TimeSpan.FromSeconds 10.)) then
                daemon.Kill()

        // A path below the folder is written with `/` whatever the platform, so that a snapshot
        // recorded on one is the snapshot of every other.
        let replacements: (string * string) list =
            [
                folder + string<char> Path.DirectorySeparatorChar, "<folder>/"
                folder + string<char> Path.AltDirectorySeparatorChar, "<folder>/"
                folder, "<folder>"
                CodeFormatter.GetVersion(), "<version>"
            ]

        let messages (recording: RecordingStream) : JsonNode =
            JsonArray(
                messagesIn recording.Recorded
                |> List.map (canonical replacements)
                |> Array.ofList
            )

        JsonObject(
            [
                Collections.Generic.KeyValuePair<string, JsonNode>("sent", messages sent)
                Collections.Generic.KeyValuePair<string, JsonNode>("received", messages received)
            ]
        )
            .ToJsonString(
                JsonSerializerOptions(WriteIndented = true, Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping)
            )
        |> String.normalizeNewLine
    finally
        Directory.Delete(folder, true)
