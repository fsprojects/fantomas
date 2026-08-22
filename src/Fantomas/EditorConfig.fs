module Fantomas.EditorConfig

open System.Collections.Concurrent
open System.Collections.Generic
open System.ComponentModel
open EditorConfig.Core
open Fantomas.Core
open Serilog

module Reflection =
    open System
    open System.Reflection
    open FSharp.Reflection

    type FSharpRecordField =
        { PropertyName: string
          Category: string option
          DisplayName: string option
          Description: string option }

    let inline getCustomAttribute<'t, 'v when 't :> Attribute and 't: null and 't: not struct>
        (projection: 't -> 'v)
        (property: PropertyInfo)
        : 'v option =
        property.GetCustomAttribute<'t>() |> Option.ofObj |> Option.map projection

    let inline getRecordFields x =
        let names =
            FSharpType.GetRecordFields(x.GetType())
            |> Seq.map (fun x ->
                { PropertyName = x.Name
                  Category = getCustomAttribute<CategoryAttribute, string> (fun a -> a.Category) x
                  DisplayName = getCustomAttribute<DisplayNameAttribute, string> (fun a -> a.DisplayName) x
                  Description = getCustomAttribute<DescriptionAttribute, string> (fun a -> a.Description) x })

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
            c.ToString())
    |> String.concat ""
    |> fun s -> s.TrimStart([| '_' |])
    |> fun name ->
        if List.contains name supportedProperties then
            name
        else
            $"fsharp_%s{name}"

let getFantomasFields (fallbackConfig: FormatConfig) =
    Reflection.getRecordFields fallbackConfig
    |> Array.map (fun (recordField, defaultValue) ->
        let editorConfigName = toEditorConfigName recordField.PropertyName

        (editorConfigName, defaultValue))

[<return: Struct>]
let (|Number|_|) (d: string) : obj voption =
    match System.Int32.TryParse(d) with
    | true, d -> ValueSome(box d)
    | _ -> ValueNone

[<return: Struct>]
let (|MultilineFormatterType|_|) (mft: string) : MultilineFormatterType voption =
    MultilineFormatterType.OfConfigString mft |> ValueOption.ofOption

[<return: Struct>]
let (|BracketStyle|_|) (bs: string) : MultilineBracketStyle voption =
    MultilineBracketStyle.OfConfigString bs |> ValueOption.ofOption

[<return: Struct>]
let (|EndOfLineStyle|_|) (eol: string) : EndOfLineStyle voption =
    EndOfLineStyle.OfConfigString eol |> ValueOption.ofOption

[<return: Struct>]
let (|Boolean|_|) (b: string) : obj voption =
    if b = "true" then ValueSome(box true)
    elif b = "false" then ValueSome(box false)
    else ValueNone

[<RequireQualifiedAccess>]
type EditorConfigProblem =
    | UnknownSetting of setting: string
    | UnrecognizedValue of setting: string * value: string

[<NoComparison>]
type EditorConfigResult =
    { Config: FormatConfig
      EditorConfigFiles: string list
      Problems: EditorConfigProblem list }

let isFantomasSetting (setting: string) : bool =
    setting.StartsWith("fsharp_", System.StringComparison.Ordinal)

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

let supportedSettings: string list =
    getFantomasFields FormatConfig.Default
    |> Array.map fst
    |> List.ofArray
    |> List.sortWith (fun left right ->
        // The settings editorconfig itself defines come first, then the ones belonging to
        // Fantomas, each group ordered the same way everywhere else here: by ordinal.
        match compare (isFantomasSetting left) (isFantomasSetting right) with
        | 0 -> System.String.CompareOrdinal(left, right)
        | difference -> difference)

let unknownFantomasSettings (editorConfigProperties: IReadOnlyDictionary<string, string>) : EditorConfigProblem list =
    editorConfigProperties.Keys
    |> Seq.filter isFantomasSetting
    |> Seq.sortWith (fun left right -> System.String.CompareOrdinal(left, right))
    |> Seq.choose (fun setting ->
        if List.contains setting supportedSettings then
            None
        else
            Some(EditorConfigProblem.UnknownSetting setting))
    |> Seq.toList

let parseOptionsFromEditorConfig
    (fallbackConfig: FormatConfig)
    (editorConfigProperties: IReadOnlyDictionary<string, string>)
    : FormatConfig * EditorConfigProblem list =
    // editorconfig keys are case insensitive. The library lowercases the ones it reads from a
    // file, but nothing lowercases a dictionary handed straight to us, so do it here rather than
    // in one caller: a key that differs only in case would otherwise match no setting and raise
    // no warning either. Folding keeps the last write, which is what editorconfig does when a
    // file sets the same key twice.
    let editorConfigProperties =
        editorConfigProperties
        |> Seq.fold (fun acc setting -> Map.add (setting.Key.ToLowerInvariant()) setting.Value acc) Map.empty
        |> Seq.map (fun setting -> setting.Key, setting.Value)
        |> readOnlyDict

    let unrecognizedValues = ResizeArray<string * string>()

    let newValues =
        getFantomasFields fallbackConfig
        |> Array.map (fun (editorConfigName, defaultValue) ->
            match editorConfigProperties.TryGetValue(editorConfigName) with
            | true, Number n -> n
            | true, Boolean b -> b
            | true, MultilineFormatterType mft -> box mft
            | true, EndOfLineStyle eol -> box eol
            | true, BracketStyle bs -> box bs
            | false, _ -> defaultValue
            | true, invalidValue ->
                if not (isSpecDefinedNonValue editorConfigName invalidValue) then
                    unrecognizedValues.Add(editorConfigName, invalidValue)

                defaultValue)

    let formatConfigType = FormatConfig.Default.GetType()

    let config =
        Microsoft.FSharp.Reflection.FSharpValue.MakeRecord(formatConfigType, newValues) :?> FormatConfig

    let unrecognized =
        unrecognizedValues
        |> Seq.sortWith (fun (left, _) (right, _) -> System.String.CompareOrdinal(left, right))
        |> Seq.map EditorConfigProblem.UnrecognizedValue
        |> Seq.toList

    config, unknownFantomasSettings editorConfigProperties @ unrecognized

/// The version without the commit hash `CodeFormatter.GetVersion` appends, so that the
/// settings a user is being pointed at are tied to a version they can act on.
let fantomasVersion: string =
    let version = CodeFormatter.GetVersion()

    match version.IndexOf('+') with
    | -1 -> version
    | index -> version.Substring(0, index)

/// Every report already written, for the lifetime of the process. Never cleared: the command line
/// reads the same `.editorconfig` again for every file it formats, and this is what keeps one typo
/// from being reported once per file. The daemon does not report through here at all.
let reportedWarnings = ConcurrentDictionary<string, unit>()

let warnOnce (log: ILogger) (message: string) : unit =
    if reportedWarnings.TryAdd(message, ()) then
        log.Warning message

let reportProblems (log: ILogger) (origin: string) (problems: EditorConfigProblem list) : unit =
    if not (List.isEmpty problems) then
        [ yield ""
          yield $"Fantomas cannot use some settings from %s{origin}:"
          for problem in problems do
              match problem with
              | EditorConfigProblem.UnknownSetting setting -> yield $"  %s{setting} is not a Fantomas setting"
              | EditorConfigProblem.UnrecognizedValue(setting, value) ->
                  yield $"  %s{setting} does not accept the value %s{value}, using the default instead"
          yield ""
          yield $"Current fantomas version (%s{fantomasVersion}) supports these .editorconfig settings:"
          for setting in supportedSettings do
              yield $"  %s{setting}"
          yield "" ]
        |> String.concat System.Environment.NewLine
        |> warnOnce log

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
        | _ -> None)
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
            { Config = config
              EditorConfigFiles = editorConfigFiles
              Problems = problems }

let readConfiguration (log: ILogger) (fsharpFile: string) : FormatConfig =
    match tryReadConfiguration fsharpFile with
    | None -> FormatConfig.Default
    | Some result ->
        let origin =
            match result.EditorConfigFiles with
            | [] -> fsharpFile
            | files -> String.concat ", " files

        reportProblems log origin result.Problems
        result.Config
