module Fantomas.Extras.EditorConfig

open System.Collections.Generic
open Fantomas.FormatConfig

module Reflection =
    open FSharp.Reflection

    let inline getRecordFields x =
        let names =
            FSharpType.GetRecordFields(x.GetType())
            |> Seq.map (fun x -> x.Name)

        let values = FSharpValue.GetRecordFields x
        Seq.zip names values |> Seq.toArray

let supportedProperties =
    [ "max_line_length"
      "indent_size"
      "end_of_line" ]

let toEditorConfigName value =
    value
    |> Seq.map
        (fun c ->
            if System.Char.IsUpper(c) then
                sprintf "_%s" (c.ToString().ToLower())
            else
                c.ToString())
    |> String.concat ""
    |> fun s -> s.TrimStart([| '_' |])
    |> fun name ->
        if List.contains name supportedProperties then
            name
        else
            sprintf "fsharp_%s" name

let private fantomasFields =
    Reflection.getRecordFields FormatConfig.Default
    |> Array.map
        (fun (propertyName, defaultValue) ->
            let editorConfigName = toEditorConfigName propertyName
            (editorConfigName, defaultValue))

let private (|Number|_|) d =
    match System.Int32.TryParse(d) with
    | true, d -> Some(box d)
    | _ -> None

let private (|MultilineFormatterType|_|) mft =
    MultilineFormatterType.OfConfigString mft

let private (|EndOfLineStyle|_|) eol = EndOfLineStyle.OfConfigString eol

let private (|Boolean|_|) b =
    if b = "true" then Some(box true)
    elif b = "false" then Some(box false)
    else None

let parseOptionsFromEditorConfig (editorConfigProperties: IReadOnlyDictionary<string, string>) : FormatConfig =
    fantomasFields
    |> Array.map
        (fun (ecn, dv) ->
            match editorConfigProperties.TryGetValue(ecn) with
            | true, Number n -> n
            | true, Boolean b -> b
            | true, MultilineFormatterType mft -> mft
            | true, EndOfLineStyle eol -> box eol
            | _ -> dv)
    |> fun newValues ->
        let formatConfigType = FormatConfig.Default.GetType()
        Microsoft.FSharp.Reflection.FSharpValue.MakeRecord(formatConfigType, newValues) :?> FormatConfig

let configToEditorConfig (config: FormatConfig) : string =
    Reflection.getRecordFields config
    |> Array.choose
        (fun (k, v) ->
            match v with
            | :? System.Boolean as b ->
                sprintf "%s=%s" (toEditorConfigName k) (if b then "true " else "false")
                |> Some
            | :? System.Int32 as i -> sprintf "%s=%d" (toEditorConfigName k) i |> Some
            | :? MultilineFormatterType as mft ->
                sprintf "%s=%s" (toEditorConfigName k) (MultilineFormatterType.ToConfigString mft)
                |> Some
            | _ -> None)
    |> String.concat "\n"

let private editorConfigParser = EditorConfig.Core.EditorConfigParser()

let tryReadConfiguration (fsharpFile: string) : FormatConfig option =
    let editorConfigSettings: EditorConfig.Core.FileConfiguration =
        editorConfigParser.Parse(fileName = fsharpFile)

    if editorConfigSettings.Properties.Count = 0 then
        None
    else
        Some(parseOptionsFromEditorConfig editorConfigSettings.Properties)

let readConfiguration (fsharpFile: string) : FormatConfig =
    tryReadConfiguration fsharpFile
    |> Option.defaultValue FormatConfig.Default
