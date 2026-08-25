module Fantomas.EditorConfig

open System.Collections.Generic
open System.ComponentModel
open EditorConfig.Core
open Fantomas.Core

module Reflection =
    open System
    open System.Reflection
    open FSharp.Reflection

    type FSharpRecordField =
        {
            PropertyName: string
            Category: string option
            DisplayName: string option
            Description: string option
        }

    let inline getCustomAttribute<'t, 'v when 't :> Attribute and 't: null and 't: not struct>
        (projection: 't -> 'v)
        (property: PropertyInfo)
        : 'v option
        =
        property.GetCustomAttribute<'t>() |> Option.ofObj |> Option.map projection

    let inline getRecordFields x =
        let names =
            FSharpType.GetRecordFields(x.GetType())
            |> Seq.map (fun x ->
                {
                    PropertyName = x.Name
                    Category = getCustomAttribute<CategoryAttribute, string> (fun a -> a.Category) x
                    DisplayName = getCustomAttribute<DisplayNameAttribute, string> (fun a -> a.DisplayName) x
                    Description = getCustomAttribute<DescriptionAttribute, string> (fun a -> a.Description) x
                }
            )

        let values = FSharpValue.GetRecordFields x
        Seq.zip names values |> Seq.toArray

let supportedProperties =
    [ "max_line_length"; "indent_size"; "end_of_line"; "insert_final_newline" ]

let toEditorConfigName value =
    value
    |> Seq.map (fun c ->
        if System.Char.IsUpper(c) then
            $"_%s{c.ToString().ToLower()}"
        else
            c.ToString()
    )
    |> String.concat ""
    |> fun s -> s.TrimStart([| '_' |])
    |> fun name ->
        if List.contains name supportedProperties then
            name
        else
            $"fsharp_%s{name}"

/// The editorconfig name of every `FormatConfig` field, in record field order. Worked out once:
/// the names cannot change while the process runs, and `parseOptionsFromEditorConfig` runs for
/// every file that gets formatted.
let settingNames: string array =
    Microsoft.FSharp.Reflection.FSharpType.GetRecordFields(typeof<FormatConfig>)
    |> Array.map (fun property -> toEditorConfigName property.Name)

/// Every field of `fallbackConfig`, paired with the editorconfig name it is written under.
let getFantomasFields (fallbackConfig: FormatConfig) : (string * obj) array =
    Microsoft.FSharp.Reflection.FSharpValue.GetRecordFields fallbackConfig
    |> Array.zip settingNames

/// Read one setting's value into the type the matching `FormatConfig` field has. Which parser
/// applies is decided by that type rather than by trying each of them in turn: `cr` means
/// something to `end_of_line` and nothing to any other setting, and trying every parser on every
/// setting made `fsharp_max_record_width = cr` fail the whole run with a message about line
/// endings.
///
/// Values are matched without regard to case, as editorconfig defines them.
let parseSettingValue (defaultValue: obj) (value: string) : obj option =
    let value = value.ToLowerInvariant()

    match defaultValue with
    | :? int ->
        match System.Int32.TryParse(value) with
        // No setting Fantomas has means anything below zero, and a run that took one would format
        // to nonsense widths without ever saying so.
        | true, number when number >= 0 -> Some(box number)
        | _ -> None
    | :? bool ->
        if value = "true" then Some(box true)
        elif value = "false" then Some(box false)
        else None
    | :? MultilineFormatterType -> MultilineFormatterType.OfConfigString value |> Option.map box
    | :? EndOfLineStyle -> EndOfLineStyle.OfConfigString value |> Option.map box
    | :? MultilineBracketStyle -> MultilineBracketStyle.OfConfigString value |> Option.map box
    | _ -> None

[<RequireQualifiedAccess>]
type EditorConfigProblem =
    | UnknownSetting of setting: string
    | UnrecognizedValue of setting: string * value: string

[<NoComparison>]
type EditorConfigResult =
    {
        Config: FormatConfig
        EditorConfigFiles: string list
        Problems: EditorConfigProblem list
    }

let isFantomasSetting (setting: string) : bool =
    setting.StartsWith("fsharp_", System.StringComparison.OrdinalIgnoreCase)

/// Values the editorconfig spec gives a meaning that is not a value. `unset` says a setting from
/// a parent file no longer applies, `indent_size = tab` says to follow `tab_width`, and
/// `max_line_length = off` says there is no limit. Fantomas cannot act on any of them, but they
/// are not mistakes, and the library derives `indent_size = tab` on its own from
/// `indent_style = tab`, so reporting them blames an author for something they never wrote.
///
/// Only these exact values are excused. Anything else, `indent_size = banana` included, is a
/// mistake and is reported like any other.
let isSpecDefinedNonValue (setting: string) (value: string) : bool =
    let value = value.ToLowerInvariant()

    value = "unset"
    || (setting = "indent_size" && value = "tab")
    || (setting = "max_line_length" && value = "off")

/// Settings are ordered the same way everywhere here: without regard to case, as they are matched.
let compareSettings (left: string) (right: string) : int =
    System.String.Compare(left, right, System.StringComparison.OrdinalIgnoreCase)

let supportedSettings: string list =
    settingNames
    |> List.ofArray
    |> List.sortWith (fun left right ->
        // The settings editorconfig itself defines come first, then the ones belonging to
        // Fantomas, each group ordered the same way everywhere else here.
        match compare (isFantomasSetting left) (isFantomasSetting right) with
        | 0 -> compareSettings left right
        | difference -> difference
    )

/// The same names again, for looking one up rather than reading the list. A `.editorconfig` is
/// read once per formatted file, so a scan of the whole list per setting is worth avoiding.
let supportedSettingLookup: HashSet<string> =
    HashSet<string>(settingNames, System.StringComparer.OrdinalIgnoreCase)

let nearestSetting (limit: int) (setting: string) : string option =
    Suggestion.nearest limit supportedSettings setting

/// How far an unprefixed key may be from a setting Fantomas has before it is read as a misspelling
/// of it rather than as something belonging to another tool.
///
/// Tighter than the distance a suggestion is offered at, and it has to be. `indent_style` is three
/// edits from `indent_size`, and it is in very nearly every `.editorconfig` ever written; warning
/// about it would put a false report in front of almost every user. Two edits reaches every
/// realistic typo, `max_line_lenght` included, and reaches nothing anyone meant to write.
[<Literal>]
let MaximumUnprefixedTypoDistance = 2

/// Whether an unprefixed key looks like a misspelling of a setting Fantomas has, rather than a
/// setting belonging to some other tool. `fsharp_`-prefixed keys are ours by construction, so this
/// only has to judge the ones that carry no prefix.
let looksLikeAMisspelling (setting: string) : bool =
    (nearestSetting MaximumUnprefixedTypoDistance setting).IsSome

let unknownFantomasSettings (settings: string seq) : EditorConfigProblem list =
    settings
    |> Seq.filter (fun setting ->
        // Anything carrying our prefix is ours to complain about. Anything without one belongs to
        // another tool unless it is close enough to one of ours to be a typo of it: a mistake in
        // `max_line_length` is silently ignored exactly as a mistake in a `fsharp_` setting was,
        // and it is the same mistake.
        isFantomasSetting setting || looksLikeAMisspelling setting
    )
    |> Seq.sortWith compareSettings
    |> Seq.choose (fun setting ->
        if supportedSettingLookup.Contains setting then
            None
        else
            Some(EditorConfigProblem.UnknownSetting setting)
    )
    |> Seq.toList

let parseOptionsFromEditorConfig
    (fallbackConfig: FormatConfig)
    (editorConfigProperties: IReadOnlyDictionary<string, string>)
    : FormatConfig * EditorConfigProblem list
    =
    // editorconfig keys are case insensitive. The library lowercases the ones it reads from a
    // file, but a dictionary an editor hands us is untouched, so match without regard to case
    // here rather than in one caller: a key that differs only in case would otherwise match no
    // setting and raise no warning either. Each entry carries the spelling it was written with,
    // so a problem names what the author typed rather than the folded form.
    let properties: Dictionary<string, struct (string * string)> =
        let properties =
            Dictionary<string, struct (string * string)>(System.StringComparer.OrdinalIgnoreCase)

        for setting in editorConfigProperties do
            properties[setting.Key] <- struct (setting.Key, setting.Value)

        properties

    let unrecognizedValues: ResizeArray<string * string> =
        ResizeArray<string * string>()

    let newValues =
        getFantomasFields fallbackConfig
        |> Array.map (fun (setting, defaultValue) ->
            match properties.TryGetValue setting with
            | false, _ -> defaultValue
            | true, struct (written, value) ->
                match parseSettingValue defaultValue value with
                | Some parsed -> parsed
                | None ->
                    if not (isSpecDefinedNonValue setting value) then
                        unrecognizedValues.Add(written, value)

                    defaultValue
        )

    let config =
        Microsoft.FSharp.Reflection.FSharpValue.MakeRecord(typeof<FormatConfig>, newValues) :?> FormatConfig

    let unrecognized =
        unrecognizedValues
        |> Seq.sortWith (fun (left, _) (right, _) -> compareSettings left right)
        |> Seq.map EditorConfigProblem.UnrecognizedValue
        |> Seq.toList

    let written = properties.Values |> Seq.map (fun (struct (written, _)) -> written)

    config, unknownFantomasSettings written @ unrecognized

let configToEditorConfig (config: FormatConfig) : string =
    Reflection.getRecordFields config
    |> Array.choose (fun (recordField, v) ->
        match v with
        | :? System.Boolean as b ->
            sprintf "%s=%s" (toEditorConfigName recordField.PropertyName) (if b then "true" else "false")
            |> Some
        | :? System.Int32 as i -> $"%s{toEditorConfigName recordField.PropertyName}=%d{i}" |> Some
        | :? MultilineFormatterType as mft ->
            $"%s{toEditorConfigName recordField.PropertyName}=%s{MultilineFormatterType.ToConfigString mft}"
            |> Some
        | :? EndOfLineStyle as eols ->
            $"%s{toEditorConfigName recordField.PropertyName}=%s{EndOfLineStyle.ToConfigString eols}"
            |> Some
        | :? MultilineBracketStyle as mbs ->
            $"%s{toEditorConfigName recordField.PropertyName}=%s{MultilineBracketStyle.ToConfigString mbs}"
            |> Some
        | _ -> None
    )
    |> String.concat "\n"

let editorConfigParser = EditorConfigParser(EditorConfigFileCache.GetOrCreate)

let tryReadConfiguration (fsharpFile: string) : EditorConfigResult option =
    let editorConfigSettings: FileConfiguration =
        editorConfigParser.Parse(fileName = fsharpFile)

    if editorConfigSettings.Properties.Count = 0 then
        None
    else
        let config, problems =
            parseOptionsFromEditorConfig FormatConfig.Default editorConfigSettings.Properties

        let editorConfigFiles =
            editorConfigSettings.EditorConfigFiles
            |> Seq.map (fun file -> System.IO.Path.GetFullPath(System.IO.Path.Combine(file.Directory, file.FileName)))
            |> Seq.toList

        Some
            {
                Config = config
                EditorConfigFiles = editorConfigFiles
                Problems = problems
            }
