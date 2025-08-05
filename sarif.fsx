#r "nuget: Thoth.Json.Newtonsoft, 0.3.2"

open System
open System.IO
open System.Text.Json
open System.Threading.Tasks

type Text = { text: string }

type Region =
    { startLine: int
      startColumn: int
      endLine: int
      endColumn: int }

type ArtifactLocation = { uri: string }

type PhysicalLocation =
    { artifactLocation: ArtifactLocation
      region: Region }

type Location = { physicalLocation: PhysicalLocation }

type Message = { text: string }

type Result =
    { ruleId: string
      ruleIndex: int
      message: Message
      locations: Location list }

type RuleShortDescription = { text: string; markdown: string }

type Rule =
    { id: string
      name: string
      shortDescription: RuleShortDescription
      helpUri: string }

type Driver =
    { name: string
      version: string
      informationUri: string
      rules: Rule list option }

type Tool = { driver: Driver }

type Invocation =
    { startTimeUtc: DateTime
      endTimeUtc: DateTime
      executionSuccessful: bool }

type Run =
    { results: Result list
      tool: Tool
      invocations: Invocation list
      columnKind: string }

type SarifLog =
    { schema: string
      version: string
      runs: Run list }

module private Encoders =
    open Thoth.Json.Core

    let textEncoder: Encoder<Text> =
        fun (t: Text) -> Encode.object [ ("text", Encode.string t.text) ]

    let regionEncoder: Encoder<Region> =
        fun (r: Region) ->
            Encode.object
                [ ("startLine", Encode.int r.startLine)
                  ("startColumn", Encode.int r.startColumn)
                  ("endLine", Encode.int r.endLine)
                  ("endColumn", Encode.int r.endColumn) ]

    let artifactLocationEncoder: Encoder<ArtifactLocation> =
        fun (al: ArtifactLocation) -> Encode.object [ ("uri", Encode.string al.uri) ]

    let physicalLocationEncoder: Encoder<PhysicalLocation> =
        fun (pl: PhysicalLocation) ->
            Encode.object
                [ ("artifactLocation", artifactLocationEncoder pl.artifactLocation)
                  ("region", regionEncoder pl.region) ]

    let locationEncoder: Encoder<Location> =
        fun (l: Location) -> Encode.object [ ("physicalLocation", physicalLocationEncoder l.physicalLocation) ]

    let messageEncoder: Encoder<Message> =
        fun (m: Message) -> Encode.object [ ("text", Encode.string m.text) ]

    let resultEncoder: Encoder<Result> =
        fun (r: Result) ->
            Encode.object
                [ ("ruleId", Encode.string r.ruleId)
                  ("ruleIndex", Encode.int r.ruleIndex)
                  ("message", messageEncoder r.message)
                  ("locations", List.map locationEncoder r.locations |> Encode.list) ]

    let ruleShortDescriptionEncoder: Encoder<RuleShortDescription> =
        fun (rsd: RuleShortDescription) ->
            Encode.object [ ("text", Encode.string rsd.text); ("markdown", Encode.string rsd.markdown) ]

    let ruleEncoder: Encoder<Rule> =
        fun (r: Rule) ->
            Encode.object
                [ ("id", Encode.string r.id)
                  ("name", Encode.string r.name)
                  ("shortDescription", ruleShortDescriptionEncoder r.shortDescription)
                  ("helpUri", Encode.string r.helpUri) ]

    let driverEncoder: Encoder<Driver> =
        fun (d: Driver) ->
            Encode.object
                [ ("name", Encode.string d.name)
                  ("version", Encode.string d.version)
                  ("informationUri", Encode.string d.informationUri)
                  ("rules",
                   match d.rules with
                   | None -> Encode.list []
                   | Some rules -> List.map ruleEncoder rules |> Encode.list) ]

    let toolEncoder: Encoder<Tool> =
        fun (t: Tool) -> Encode.object [ ("driver", driverEncoder t.driver) ]

    let invocationEncoder: Encoder<Invocation> =
        fun (i: Invocation) ->
            Encode.object
                [ ("startTimeUtc", Encode.string (i.startTimeUtc.ToString("o"))) // ISO 8601 format
                  ("endTimeUtc", Encode.string (i.endTimeUtc.ToString("o")))
                  ("executionSuccessful", Encode.bool i.executionSuccessful) ]

    let runEncoder: Encoder<Run> =
        fun (r: Run) ->
            Encode.object
                [ ("results", List.map resultEncoder r.results |> Encode.list)
                  ("tool", toolEncoder r.tool)
                  ("invocations", List.map invocationEncoder r.invocations |> Encode.list)
                  ("columnKind", Encode.string r.columnKind) ]

    let sarifLogEncoder: Encoder<SarifLog> =
        fun (log: SarifLog) ->
            Encode.object
                [ ("$schema", Encode.string log.schema)
                  ("version", Encode.string log.version)
                  ("runs", List.map runEncoder log.runs |> Encode.list) ]

module private Decoders =
    open Thoth.Json.Core

    let textDecoder: Decoder<Text> =
        Decode.object (fun get -> { text = get.Required.Field "text" Decode.string })

    let regionDecoder: Decoder<Region> =
        Decode.object (fun get ->
            { startLine = get.Required.Field "startLine" Decode.int
              startColumn = get.Required.Field "startColumn" Decode.int
              endLine = get.Required.Field "endLine" Decode.int
              endColumn = get.Required.Field "endColumn" Decode.int })

    let artifactLocationDecoder: Decoder<ArtifactLocation> =
        Decode.object (fun get -> { uri = get.Required.Field "uri" Decode.string })

    let physicalLocationDecoder: Decoder<PhysicalLocation> =
        Decode.object (fun get ->
            { artifactLocation = get.Required.Field "artifactLocation" artifactLocationDecoder
              region = get.Required.Field "region" regionDecoder })

    let locationDecoder: Decoder<Location> =
        Decode.object (fun get -> { physicalLocation = get.Required.Field "physicalLocation" physicalLocationDecoder })

    let messageDecoder: Decoder<Message> =
        Decode.object (fun get -> { text = get.Required.Field "text" Decode.string })

    let resultDecoder: Decoder<Result> =
        Decode.object (fun get ->
            { ruleId = get.Required.Field "ruleId" Decode.string
              ruleIndex = get.Required.Field "ruleIndex" Decode.int
              message = get.Required.Field "message" messageDecoder
              locations = get.Required.Field "locations" (Decode.list locationDecoder) })

    let ruleShortDescriptionDecoder: Decoder<RuleShortDescription> =
        Decode.object (fun get ->
            { text = get.Required.Field "text" Decode.string
              markdown = get.Required.Field "markdown" Decode.string })

    let ruleDecoder: Decoder<Rule> =
        Decode.object (fun get ->
            { id = get.Required.Field "id" Decode.string
              name = get.Required.Field "name" Decode.string
              shortDescription = get.Required.Field "shortDescription" ruleShortDescriptionDecoder
              helpUri = get.Required.Field "helpUri" Decode.string })

    let driverDecoder: Decoder<Driver> =
        Decode.object (fun get ->
            { name = get.Required.Field "name" Decode.string
              version = get.Required.Field "version" Decode.string
              informationUri = get.Required.Field "informationUri" Decode.string
              rules = get.Optional.Field "rules" (Decode.list ruleDecoder) })

    let toolDecoder: Decoder<Tool> =
        Decode.object (fun get -> { driver = get.Required.Field "driver" driverDecoder })

    let invocationDecoder: Decoder<Invocation> =
        Decode.object (fun get ->
            { startTimeUtc = get.Required.Field "startTimeUtc" Decode.datetimeUtc
              endTimeUtc = get.Required.Field "endTimeUtc" Decode.datetimeUtc
              executionSuccessful = get.Required.Field "executionSuccessful" Decode.bool })

    let runDecoder: Decoder<Run> =
        Decode.object (fun get ->
            { results = get.Required.Field "results" (Decode.list resultDecoder)
              tool = get.Required.Field "tool" toolDecoder
              invocations = get.Required.Field "invocations" (Decode.list invocationDecoder)
              columnKind = get.Required.Field "columnKind" Decode.string })

    let sarifLogDecoder: Decoder<SarifLog> =
        Decode.object (fun get ->
            { schema = get.Required.Field "$schema" Decode.string
              version = get.Required.Field "version" Decode.string
              runs = get.Required.Field "runs" (Decode.list runDecoder) })

let private readSarif (json: string) : Result<SarifLog, string> =
    match Thoth.Json.Newtonsoft.Decode.fromString Decoders.sarifLogDecoder json with
    | Ok sarifLog -> Ok sarifLog
    | Error err -> Error($"Failed to decode, got %A{err}")

let private writeSarif (sarifLog: SarifLog) : string =
    Encoders.sarifLogEncoder sarifLog |> Thoth.Json.Newtonsoft.Encode.toString 4

let mergeSarifFiles _ =
    task {
        let mergedPath =
            Path.Combine(__SOURCE_DIRECTORY__, "analysisreports", "merged.sarif")

        if Path.Exists(mergedPath) then
            File.Delete(mergedPath)

        let! sarifFiles =
            Directory.GetFiles("analysisreports", "*.sarif")
            |> Seq.map (fun path ->
                task {
                    let! sarifContent = File.ReadAllTextAsync(path)
                    let sarifResult = readSarif sarifContent

                    match sarifResult with
                    | Error e ->
                        eprintfn $"%A{e}"
                        return exit 1
                    | Ok sarif -> return path, sarif
                })
            |> Task.WhenAll

        if Array.isEmpty sarifFiles then
            printfn "No sarif files could be merged"
        else
            let firstSarif = snd (sarifFiles.[0])
            let firstRun = firstSarif.runs.[0]

            let results = ResizeArray()
            let rules = ResizeArray()

            for _, sarif in sarifFiles do
                for run in sarif.runs do
                    results.AddRange(run.results)

                    match run.tool.driver.rules with
                    | None -> ()
                    | Some rulesList -> rules.AddRange(rulesList)


            let combined: SarifLog =
                {
                  // I don't know why firstSarif.schema is null
                  schema = "https://schemastore.azurewebsites.net/schemas/json/sarif-2.1.0-rtm.6.json"
                  version = firstSarif.version
                  runs =
                    [ { tool =
                          { firstRun.tool with
                              driver =
                                  { firstRun.tool.driver with
                                      rules = Some(List.ofSeq rules) } }
                        invocations = firstRun.invocations
                        columnKind = firstRun.columnKind
                        results = List.ofSeq results } ] }

            sarifFiles |> Array.iter (fun (path, _) -> File.Delete(path))

            let mergedStream = File.OpenWrite("analysisreports/merged.sarif")
            let combinedJson = writeSarif combined
            do! mergedStream.WriteAsync(System.Text.Encoding.UTF8.GetBytes(combinedJson))
            do! mergedStream.FlushAsync()
            mergedStream.Close()
            printfn $"Successfully merged %d{sarifFiles.Length} SARIF files"
    }
