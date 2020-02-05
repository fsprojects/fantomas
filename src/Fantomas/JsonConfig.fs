module internal Fantomas.JsonConfig

open System
open System.Text.RegularExpressions

let parseOptionsFromJson json =
    let results =
        Regex.Replace(json, "\s|{|}", String.Empty).Split([|','|])
        |> Array.map (fun line -> line, line.Split([|':'|]))
        |> Array.choose (fun (line, pieces) ->
            if Array.length pieces = 2 && pieces.[0] <> "$schema"
            then Some(line, pieces.[0], pieces.[1])
            else None)
        |> Array.map(fun (line, key,value) ->
            ConfigFile.processSetting line key value
        )

    let options = Array.choose (function | Ok r -> Some r | _ -> None) results
    let warnings = Array.choose (function | Error r -> Some r | _ -> None) results
    options, warnings
