module internal Fantomas.EditorConfig

open Fantomas.FormatConfig

let private snakeCase value =
    value
    |> Seq.map (fun c -> if System.Char.IsUpper(c) then sprintf "_%s" (c.ToString().ToLower()) else c.ToString())
    |> String.concat ""
    |> fun s -> s.TrimStart([|'_'|])

let private fantomasFields =
    Reflection.getRecordFields FormatConfig.Default
    |> Array.map (fun (propertyName, defaultValue) ->
        let editorConfigName = snakeCase propertyName
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
        if editorConfig.Properties.ContainsKey(ecn) then
            let ecv = editorConfig.Properties.[ecn]
            match ecv with
            | Number n -> box n
            | Boolean b -> box b
            | _ -> dv
        else
            dv)
    |> fun newValues ->
        let formatConfigType = FormatConfig.Default.GetType()
        Microsoft.FSharp.Reflection.FSharpValue.MakeRecord(formatConfigType, newValues) :?> FormatConfig
