module Fantomas.EditorConfigFiles

open System.Collections.Generic
open EditorConfig.Core
open Fantomas.Core
open Fantomas.EditorConfig

[<NoComparison>]
type EditorConfigResult =
    {
        Config: FormatConfig
        EditorConfigFiles: string list
        Problems: EditorConfigProblem list
    }

/// One parser for the process, and with it one cache of the `.editorconfig` files it has read.
let editorConfigParser: EditorConfigParser = EditorConfigParser()

/// Where an `.editorconfig` file lives, as one absolute path.
let editorConfigFilePath (file: IEditorConfigFile) : string =
    System.IO.Path.GetFullPath(System.IO.Path.Combine(file.Directory, file.FileName))

let tryReadConfiguration (fsharpFile: string) : EditorConfigResult option =
    let editorConfigSettings: FileConfiguration =
        editorConfigParser.Parse(fileName = fsharpFile)

    if editorConfigSettings.Properties.Count = 0 then
        None
    else

    let config, problems =
        parseOptionsFromEditorConfig FormatConfig.Default editorConfigSettings.Properties

    let editorConfigFiles: string list =
        editorConfigSettings.EditorConfigFiles
        |> Seq.map editorConfigFilePath
        |> Seq.toList

    Some
        {
            Config = config
            EditorConfigFiles = editorConfigFiles
            Problems = problems
        }

type ResolvedSetting =
    {
        Setting: string
        Value: string
        SetBy: string option
    }

[<NoComparison>]
type ResolvedConfig =
    {
        Config: FormatConfig
        Settings: ResolvedSetting list
        EditorConfigFiles: string list
        Problems: EditorConfigProblem list
    }

    member this.FromEditorConfig: ResolvedSetting list =
        this.Settings
        |> List.filter (fun (setting: ResolvedSetting) -> setting.SetBy.IsSome)

/// Which `.editorconfig` of the chain set each key.
///
/// Worked out by reading the chain one file longer at a time and looking at what each addition
/// changed, rather than by reading each file alone. Alone is the wrong question: whether a section
/// of a file applies to a path, and which of two sections of the same file wins, are the library's
/// rules and not ours to restate. Asked this way the library answers both, and a key whose value a
/// nearer file merely repeats is credited to the nearer file, which is the one that would have to
/// change to change it.
let settingOrigins (fsharpFile: string) : Map<string, string> =
    let chain: System.Collections.Generic.IList<EditorConfigFile> =
        editorConfigParser.GetConfigurationFilesTillRoot fsharpFile

    let mutable previous: Map<string, string> = Map.empty
    let mutable origins: Map<string, string> = Map.empty

    for index in 0 .. chain.Count - 1 do
        let properties: IReadOnlyDictionary<string, string> =
            editorConfigParser.Parse(fsharpFile, Seq.truncate (index + 1) chain).Properties

        let current: Map<string, string> =
            properties |> Seq.map (fun setting -> setting.Key, setting.Value) |> Map.ofSeq

        let file: string = editorConfigFilePath chain.[index]

        for setting in current do
            if Map.tryFind setting.Key previous <> Some setting.Value then
                origins <- Map.add setting.Key file origins

        previous <- current

    origins

let resolveConfiguration (fsharpFile: string) : ResolvedConfig =
    let editorConfigSettings: FileConfiguration =
        editorConfigParser.Parse(fileName = fsharpFile)

    let config, problems =
        parseOptionsFromEditorConfig FormatConfig.Default editorConfigSettings.Properties

    let editorConfigFiles: string list =
        editorConfigSettings.EditorConfigFiles
        |> Seq.map editorConfigFilePath
        |> Seq.toList

    // A second walk of the chain, and only where there is a chain to walk. It costs what the first
    // one costs, times the number of files above this one, and it buys the only thing the first
    // cannot say: which of those files a value came from.
    let origins: Map<string, string> =
        if List.isEmpty editorConfigFiles then
            Map.empty
        else
            settingOrigins fsharpFile

    // A value Fantomas could not read leaves the default in place, so the file that wrote it did
    // not decide what will be used and is not named as having.
    let unreadable: HashSet<string> =
        problems
        |> List.choose (fun (problem: EditorConfigProblem) ->
            match problem with
            | EditorConfigProblem.UnrecognizedValue(setting, _) -> Some setting
            | EditorConfigProblem.UnknownSetting _ -> None
        )
        |> fun (settings: string list) -> HashSet<string>(settings, System.StringComparer.OrdinalIgnoreCase)

    let resolved: Map<string, string> = Map.ofList (settingValues config)

    let settings: ResolvedSetting list =
        supportedSettings
        |> List.choose (fun (setting: string) ->
            Map.tryFind setting resolved
            |> Option.map (fun (value: string) ->
                {
                    Setting = setting
                    Value = value
                    SetBy =
                        if unreadable.Contains setting then
                            None
                        else
                            Map.tryFind setting origins
                }
            )
        )

    {
        Config = config
        Settings = settings
        EditorConfigFiles = editorConfigFiles
        Problems = problems
    }

let withoutEditorConfig (config: FormatConfig) : ResolvedConfig =
    // `supportedSettings` order, the same as `resolveConfiguration` builds, so that a report reads
    // the same however the configuration in front of it was arrived at. `settingValues` is in
    // record field order, which is the order the type happens to be declared in and is nobody's
    // idea of a reading order.
    let resolved: Map<string, string> = Map.ofList (settingValues config)

    {
        Config = config
        Settings =
            supportedSettings
            |> List.choose (fun (setting: string) ->
                Map.tryFind setting resolved
                |> Option.map (fun (value: string) ->
                    {
                        Setting = setting
                        Value = value
                        SetBy = None
                    }
                )
            )
        EditorConfigFiles = []
        Problems = []
    }
