module internal Fantomas.DaemonJson

open System
open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Serialization
open System.Text.Json.Serialization.Metadata
open Fantomas.Client.Contracts
open Fantomas.Client.LSPFantomasServiceTypes
open StreamJsonRpc.Protocol

/// A property looked up without regard to case, as Newtonsoft.Json reads them.
let tryProperty (element: JsonElement) (name: string) : JsonElement option =
    if element.ValueKind <> JsonValueKind.Object then
        None
    else

    element.EnumerateObject()
    |> Seq.tryFind (fun property -> property.Name.Equals(name, StringComparison.OrdinalIgnoreCase))
    |> Option.map (fun property -> property.Value)
    |> Option.filter (fun value -> value.ValueKind <> JsonValueKind.Null)

let readString (element: JsonElement) (name: string) : string =
    match tryProperty element name with
    | Some value when value.ValueKind = JsonValueKind.String -> value.GetString()
    | Some value -> value.GetRawText()
    | None -> null

let readInt (element: JsonElement) (name: string) : int =
    match tryProperty element name with
    | Some value -> value.GetInt32()
    | None -> 0

/// An F# option as Newtonsoft.Json writes one: `null`, or `{"Case": "Some", "Fields": [value]}`.
let readOption (read: JsonElement -> 'T) (element: JsonElement) (name: string) : 'T option =
    match tryProperty element name with
    | None -> None
    | Some option ->

    match readString option "Case", tryProperty option "Fields" with
    | "Some", Some fields when fields.ValueKind = JsonValueKind.Array && fields.GetArrayLength() = 1 ->
        Some(read fields.[0])
    | _ -> None

let readConfig (element: JsonElement) : IReadOnlyDictionary<string, string> =
    let settings: Dictionary<string, string> = Dictionary<string, string>()

    for property in element.EnumerateObject() do
        let value: string =
            match property.Value.ValueKind with
            | JsonValueKind.String -> property.Value.GetString()
            | _ -> property.Value.GetRawText()

        settings.[property.Name] <- value

    settings :> IReadOnlyDictionary<string, string>

let readCursor (element: JsonElement) : FormatCursorPosition =
    FormatCursorPosition(readInt element "Line", readInt element "Column")

let readRange (element: JsonElement) : FormatSelectionRange =
    FormatSelectionRange(
        readInt element "StartLine",
        readInt element "StartColumn",
        readInt element "EndLine",
        readInt element "EndColumn"
    )

/// A union case as Newtonsoft.Json writes one. `Case` goes first: Newtonsoft.Json needs it before
/// `Fields` to know what the fields are.
let writeCase (writer: Utf8JsonWriter) (caseName: string) (writeFields: Utf8JsonWriter -> unit) : unit =
    writer.WriteStartObject()
    writer.WriteString("Case", caseName)
    writer.WriteStartArray("Fields")
    writeFields writer
    writer.WriteEndArray()
    writer.WriteEndObject()

let writeNullableString (writer: Utf8JsonWriter) (value: string) : unit =
    if isNull value then
        writer.WriteNullValue()
    else
        writer.WriteStringValue value

/// A record property, left out when it is `null`, as `StreamJsonRpc` configures Newtonsoft.Json to.
let writeStringProperty (writer: Utf8JsonWriter) (name: string) (value: string) : unit =
    if not (isNull value) then
        writer.WriteString(name, value)

/// Every type here is only read or only written: the daemon never sends a request or reads a
/// response. Asking for the other direction is a mistake to hear about.
let oneWay (typeName: string) : 'T =
    raise (NotSupportedException $"The daemon has no reason to convert a %s{typeName} in this direction.")

/// The request itself. `Fantomas.Client` sends it as the parameter object, but a request that
/// passes it as the only positional parameter is handed over as the whole `params` array.
let requestIn (element: JsonElement) : JsonElement =
    if element.ValueKind = JsonValueKind.Array && element.GetArrayLength() = 1 then
        element.[0]
    else
        element

type FormatDocumentRequestConverter() =
    inherit JsonConverter<FormatDocumentRequest>()

    override _.Read(reader: byref<Utf8JsonReader>, _: Type, _: JsonSerializerOptions) : FormatDocumentRequest =
        use document = JsonDocument.ParseValue(&reader)
        let element: JsonElement = requestIn document.RootElement

        {
            SourceCode = readString element "SourceCode"
            FilePath = readString element "FilePath"
            Config = readOption readConfig element "Config"
            Cursor = readOption readCursor element "Cursor"
        }

    override _.Write(_: Utf8JsonWriter, _: FormatDocumentRequest, _: JsonSerializerOptions) : unit =
        oneWay (nameof FormatDocumentRequest)

type FormatSelectionRequestConverter() =
    inherit JsonConverter<FormatSelectionRequest>()

    override _.Read(reader: byref<Utf8JsonReader>, _: Type, _: JsonSerializerOptions) : FormatSelectionRequest =
        use document = JsonDocument.ParseValue(&reader)
        let element: JsonElement = requestIn document.RootElement

        {
            SourceCode = readString element "SourceCode"
            FilePath = readString element "FilePath"
            Config = readOption readConfig element "Config"
            Range =
                match tryProperty element "Range" with
                | Some range -> readRange range
                | None -> FormatSelectionRange(0, 0, 0, 0)
        }

    override _.Write(_: Utf8JsonWriter, _: FormatSelectionRequest, _: JsonSerializerOptions) : unit =
        oneWay (nameof FormatSelectionRequest)

type FormatDocumentResponseConverter() =
    inherit JsonConverter<FormatDocumentResponse>()

    override _.Read(_: byref<Utf8JsonReader>, _: Type, _: JsonSerializerOptions) : FormatDocumentResponse =
        oneWay (nameof FormatDocumentResponse)

    override _.Write(writer: Utf8JsonWriter, response: FormatDocumentResponse, _: JsonSerializerOptions) : unit =
        match response with
        | FormatDocumentResponse.Formatted(filename, formattedContent, cursor) ->
            writeCase
                writer
                "Formatted"
                (fun writer ->
                    writeNullableString writer filename
                    writeNullableString writer formattedContent

                    match cursor with
                    | None -> writer.WriteNullValue()
                    | Some cursor ->

                    writeCase
                        writer
                        "Some"
                        (fun writer ->
                            writer.WriteStartObject()
                            writer.WriteNumber("Line", cursor.Line)
                            writer.WriteNumber("Column", cursor.Column)
                            writer.WriteEndObject()
                        )
                )
        | FormatDocumentResponse.Unchanged filename ->
            writeCase writer "Unchanged" (fun writer -> writeNullableString writer filename)
        | FormatDocumentResponse.Error(filename, formattingError) ->
            writeCase
                writer
                "Error"
                (fun writer ->
                    writeNullableString writer filename
                    writeNullableString writer formattingError
                )
        | FormatDocumentResponse.IgnoredFile filename ->
            writeCase writer "IgnoredFile" (fun writer -> writeNullableString writer filename)

type FormatSelectionResponseConverter() =
    inherit JsonConverter<FormatSelectionResponse>()

    override _.Read(_: byref<Utf8JsonReader>, _: Type, _: JsonSerializerOptions) : FormatSelectionResponse =
        oneWay (nameof FormatSelectionResponse)

    override _.Write(writer: Utf8JsonWriter, response: FormatSelectionResponse, _: JsonSerializerOptions) : unit =
        match response with
        | FormatSelectionResponse.Formatted(filename, formattedContent, range) ->
            writeCase
                writer
                "Formatted"
                (fun writer ->
                    writeNullableString writer filename
                    writeNullableString writer formattedContent
                    writer.WriteStartObject()
                    writer.WriteNumber("StartLine", range.StartLine)
                    writer.WriteNumber("StartColumn", range.StartColumn)
                    writer.WriteNumber("EndLine", range.EndLine)
                    writer.WriteNumber("EndColumn", range.EndColumn)
                    writer.WriteEndObject()
                )
        | FormatSelectionResponse.Error(filename, formattingError) ->
            writeCase
                writer
                "Error"
                (fun writer ->
                    writeNullableString writer filename
                    writeNullableString writer formattingError
                )

type ConfigurationWarningConverter() =
    inherit JsonConverter<ConfigurationWarning>()

    override _.Read(_: byref<Utf8JsonReader>, _: Type, _: JsonSerializerOptions) : ConfigurationWarning =
        oneWay (nameof ConfigurationWarning)

    override _.Write(writer: Utf8JsonWriter, warning: ConfigurationWarning, _: JsonSerializerOptions) : unit =
        writer.WriteStartObject()
        writeStringProperty writer "Version" warning.Version
        writeStringProperty writer "FilePath" warning.FilePath

        if not (isNull warning.EditorConfigFiles) then
            writer.WriteStartArray("EditorConfigFiles")

            for file in warning.EditorConfigFiles do
                writeNullableString writer file

            writer.WriteEndArray()

        if not (isNull warning.Problems) then
            writer.WriteStartArray("Problems")

            for problem in warning.Problems do
                writer.WriteStartObject()
                writer.WriteNumber("Code", problem.Code)
                writer.WriteNumber("Source", problem.Source)
                writeStringProperty writer "Setting" problem.Setting
                writeStringProperty writer "Value" problem.Value
                writer.WriteEndObject()

            writer.WriteEndArray()

        writer.WriteEndObject()

/// What `StreamJsonRpc` sends as the `data` of an error response when a method throws. It asks the
/// options for user data for it, and the ones it has built in for its own types are private.
type CommonErrorDataConverter() =
    inherit JsonConverter<CommonErrorData>()

    override _.Read(_: byref<Utf8JsonReader>, _: Type, _: JsonSerializerOptions) : CommonErrorData =
        oneWay (nameof CommonErrorData)

    override this.Write(writer: Utf8JsonWriter, error: CommonErrorData, options: JsonSerializerOptions) : unit =
        writer.WriteStartObject()
        writeStringProperty writer "type" error.TypeName
        writeStringProperty writer "message" error.Message
        writeStringProperty writer "stack" error.StackTrace
        writer.WriteNumber("code", error.HResult)

        if not (isNull error.Inner) then
            writer.WritePropertyName "inner"
            this.Write(writer, error.Inner, options)

        writer.WriteEndObject()

/// Hands out a converter per type the daemon sends or receives. `CreateValueInfo` builds the
/// metadata from the converter alone, which is what keeps this free of reflection.
type DaemonTypeInfoResolver() =
    interface IJsonTypeInfoResolver with
        member _.GetTypeInfo(requested: Type, options: JsonSerializerOptions) : JsonTypeInfo =
            if requested = typeof<string> then
                JsonMetadataServices.CreateValueInfo<string>(options, JsonMetadataServices.StringConverter)
            elif requested = typeof<FormatDocumentRequest> then
                JsonMetadataServices.CreateValueInfo<FormatDocumentRequest>(options, FormatDocumentRequestConverter())
            elif requested = typeof<FormatSelectionRequest> then
                JsonMetadataServices.CreateValueInfo<FormatSelectionRequest>(options, FormatSelectionRequestConverter())
            elif requested = typeof<FormatDocumentResponse> then
                JsonMetadataServices.CreateValueInfo<FormatDocumentResponse>(options, FormatDocumentResponseConverter())
            elif requested = typeof<FormatSelectionResponse> then
                JsonMetadataServices.CreateValueInfo<FormatSelectionResponse>(
                    options,
                    FormatSelectionResponseConverter()
                )
            elif requested = typeof<ConfigurationWarning> then
                JsonMetadataServices.CreateValueInfo<ConfigurationWarning>(options, ConfigurationWarningConverter())
            elif requested = typeof<CommonErrorData> then
                JsonMetadataServices.CreateValueInfo<CommonErrorData>(options, CommonErrorDataConverter())
            else
                null

let serializerOptions () : JsonSerializerOptions =
    JsonSerializerOptions(TypeInfoResolver = DaemonTypeInfoResolver())
