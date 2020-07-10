module internal Fantomas.EditorConfig

open Fantomas.FormatConfig

let supportedProperties = ["max_line_length";"indent_size"]

let private toEditorConfigName value =
    value
    |> Seq.map (fun c -> if System.Char.IsUpper(c) then sprintf "_%s" (c.ToString().ToLower()) else c.ToString())
    |> String.concat ""
    |> fun s -> s.TrimStart([|'_'|])
    |> fun name ->
        if List.contains name supportedProperties then
            name
        else
            sprintf "fsharp_%s" name

let private fantomasFields =
    Reflection.getRecordFields FormatConfig.Default
    |> Array.map (fun (propertyName, defaultValue) ->
        let editorConfigName = toEditorConfigName propertyName
        (editorConfigName, defaultValue))

let private (|Number|_|) d =
    match System.Int32.TryParse(d) with
    | true, d -> Some (box d)
    | _ -> None
let private (|Boolean|_|) b =
    if b = "true" then Some (box true)
    elif b = "false" then Some (box false)
    else None

let parseOptionsFromEditorConfig (editorConfig: Fantomas.EditorConfig.Core.FileConfiguration) =
    fantomasFields
    |> Array.map (fun (ecn, dv) ->
        match editorConfig.Properties.TryGetValue(ecn) with
        | true, Number n -> n
        | true, Boolean b -> b
        | _ -> dv)
    |> fun newValues ->
        let formatConfigType = FormatConfig.Default.GetType()
        Microsoft.FSharp.Reflection.FSharpValue.MakeRecord(formatConfigType, newValues) :?> FormatConfig

let configToEditorConfig (config: FormatConfig): string =
    Reflection.getRecordFields config
    |> Array.choose (fun (k,v) ->
        match v with
        | :? System.Boolean as b ->
            sprintf "%s=%s" (toEditorConfigName k) (if b then "true " else "false")
            |> Some
        | :? System.Int32 as i ->
            sprintf "%s=%d" (toEditorConfigName k) i
            |> Some
        | _ -> None)
    |> String.concat "\n"