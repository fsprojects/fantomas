module internal Fantomas.EditorConfig

open Fantomas.FormatConfig

let private fantomasFields =
    Reflection.getRecordFields FormatConfig.Default
    |> Array.map (fun (propertyName, defaultValue) ->
        let editorConfigName =
            if (propertyName = "IndentSpaceNum") then
                "indent_size"
            elif (propertyName = "PageWidth") then
                "max_line_length"
            else
                propertyName
                |> Seq.map (fun c ->
                    if System.Char.IsUpper(c) then sprintf "_%c" c else c.ToString())
                |> Seq.skip 1
                |> string
        (editorConfigName, defaultValue))

let parseOptionsFromEditorConfig (editorConfig: Fantomas.EditorConfig.Core.FileConfiguration) =
    let config =
        fantomasFields
        |> Array.map (fun (ecn, dv) ->
            if editorConfig.Properties.ContainsKey(ecn) then
                let ecv = editorConfig.Properties.[ecn]
                match bool.TryParse(ecv) with
                | (true, v) -> box v
                | _ ->
                    match System.Int32.TryParse(ecv) with
                    | (true, i) -> box i
                    | _ -> dv
            else
                dv)
        |> fun newValues ->
            let formatConfigType = FormatConfig.Default.GetType()
            Microsoft.FSharp.Reflection.FSharpValue.MakeRecord(formatConfigType, newValues) :?> FormatConfig

    let unknownSettings =
        editorConfig.Properties.Keys
        |> Seq.filter (fun k -> not (Seq.exists (fst >> ((=) k)) fantomasFields))
        |> Seq.toList

    config, unknownSettings
