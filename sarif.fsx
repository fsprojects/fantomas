module Sarif

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading.Tasks

[<CLIMutable>]
type Text =
    { [<field: JsonPropertyName("text")>]
      text: string }

[<CLIMutable>]
type Region =
    { [<field: JsonPropertyName("startLine")>]
      startLine: int
      [<field: JsonPropertyName("startColumn")>]
      startColumn: int
      [<field: JsonPropertyName("endLine")>]
      endLine: int
      [<field: JsonPropertyName("endColumn")>]
      endColumn: int }

[<CLIMutable>]
type ArtifactLocation =
    { [<field: JsonPropertyName("uri")>]
      uri: string }

[<CLIMutable>]
type PhysicalLocation =
    { [<field: JsonPropertyName("artifactLocation")>]
      artifactLocation: ArtifactLocation
      [<field: JsonPropertyName("region")>]
      region: Region }

[<CLIMutable>]
type Location =
    { [<field: JsonPropertyName("physicalLocation")>]
      physicalLocation: PhysicalLocation }

[<CLIMutable>]
type Message =
    { [<field: JsonPropertyName("text")>]
      text: string }

[<CLIMutable>]
type Result =
    { [<field: JsonPropertyName("ruleId")>]
      ruleId: string
      [<field: JsonPropertyName("ruleIndex")>]
      ruleIndex: int
      [<field: JsonPropertyName("message")>]
      message: Message
      [<field: JsonPropertyName("locations")>]
      locations: Location list }

[<CLIMutable>]
type RuleShortDescription =
    { [<field: JsonPropertyName("text")>]
      text: string
      [<field: JsonPropertyName("markdown")>]
      markdown: string }

[<CLIMutable>]
type Rule =
    { [<field: JsonPropertyName("id")>]
      id: string
      [<field: JsonPropertyName("name")>]
      name: string
      [<field: JsonPropertyName("shortDescription")>]
      shortDescription: RuleShortDescription
      [<field: JsonPropertyName("helpUri")>]
      helpUri: string }

[<CLIMutable>]
type Driver =
    { [<field: JsonPropertyName("name")>]
      name: string
      [<field: JsonPropertyName("version")>]
      version: string
      [<field: JsonPropertyName("informationUri")>]
      informationUri: string
      [<field: JsonPropertyName("rules")>]
      rules: Rule list }

[<CLIMutable>]
type Tool =
    { [<field: JsonPropertyName("driver")>]
      driver: Driver }

[<CLIMutable>]
type Invocation =
    { [<field: JsonPropertyName("startTimeUtc")>]
      startTimeUtc: DateTime
      [<field: JsonPropertyName("endTimeUtc")>]
      endTimeUtc: DateTime
      [<field: JsonPropertyName("executionSuccessful")>]
      executionSuccessful: bool }

[<CLIMutable>]
type Run =
    { [<field: JsonPropertyName("results")>]
      results: Result list
      [<field: JsonPropertyName("tool")>]
      tool: Tool
      [<field: JsonPropertyName("invocations")>]
      invocations: Invocation list
      [<field: JsonPropertyName("columnKind")>]
      columnKind: string }

[<CLIMutable>]
type SarifLog =
    {
      // This field needs JsonPropertyName because F# doesn't allow '$' in identifiers.
      [<field: JsonPropertyName("$schema")>]
      schema: string
      [<field: JsonPropertyName("version")>]
      version: string
      [<field: JsonPropertyName("runs")>]
      runs: Run list }

let private options = JsonSerializerOptions()

let private readSarif (json: System.IO.Stream) : System.Threading.Tasks.ValueTask<SarifLog> =
    JsonSerializer.DeserializeAsync<SarifLog>(json, options)

let private writeSarif (json: System.IO.Stream) (sarifLog: SarifLog) : Threading.Tasks.Task =
    JsonSerializer.SerializeAsync(json, sarifLog, options)

let mergeSarifFiles _ =
    task {
        let! sarifFiles =
            Directory.GetFiles("analysisreports", "*.sarif")
            |> Seq.map (fun path ->
                task {
                    let sarifContent = File.OpenRead(path)
                    let! sarif = readSarif sarifContent
                    return path, sarif
                })
            |> Task.WhenAll

        if Array.isEmpty sarifFiles then
            printfn "No sarif files could be merged"
        else
            let firstSarif = snd (sarifFiles.[0])
            let firstRun = firstSarif.runs.[0]

            let results =
                sarifFiles
                |> Array.fold
                    (fun acc (_, sarif: SarifLog) ->
                        sarif.runs
                        |> List.collect (fun (r: Run) -> r.results)
                        |> List.toArray
                        |> Array.append acc)
                    [||]
                |> List.ofArray

            let combined: SarifLog =
                {
                  // I don't know why firstSarif.schema is null
                  schema = "https://schemastore.azurewebsites.net/schemas/json/sarif-2.1.0-rtm.6.json"
                  version = firstSarif.version
                  runs =
                    [ { tool = firstRun.tool
                        invocations = firstRun.invocations
                        columnKind = firstRun.columnKind
                        results = results } ] }

            sarifFiles |> Array.iter (fun (path, _) -> File.Delete(path))

            let mergedStream = File.OpenWrite("analysisreports/merged.sarif")
            do! writeSarif mergedStream combined
            do! mergedStream.FlushAsync()
    }
