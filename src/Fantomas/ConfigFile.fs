module internal Fantomas.ConfigFile

open System.IO
open Fantomas.FormatConfig
open Fantomas.Version

let jsonConfigFileName = "fantomas-config.json"
let editorConfigFileName = ".editor-config"

let private allowedFileNames = [jsonConfigFileName; editorConfigFileName]

let rec private getParentFolders acc current =
    let parent = Directory.GetParent(current) |> Option.ofObj
    match parent with
    | Some p -> getParentFolders (current::acc) p.FullName
    | None -> current::acc

/// Returns all the found configuration files for the given path
/// fileOrFolder can be a concrete json file or a directory path
let rec findConfigurationFiles fileOrFolder : string list =
    let findConfigInFolder folderPath =
        allowedFileNames
        |> List.map (fun fn -> Path.Combine(folderPath, fn))
        |> List.filter (File.Exists)

    if Path.GetExtension(fileOrFolder) = "" && Directory.Exists fileOrFolder then
        getParentFolders [] fileOrFolder
        |> List.collect findConfigInFolder
    elif File.Exists(fileOrFolder) then
        let parentFolder =
            Directory.GetParent(Path.GetDirectoryName(fileOrFolder))
            |> Option.ofObj
        match parentFolder with
        | Some pf -> findConfigurationFiles pf.FullName @ [fileOrFolder]
        | None -> [fileOrFolder]
    else
        []

let makeWarningLocationAware configPath warning =
    sprintf "%s, in %s" warning configPath

let private fantomasFields = Reflection.getRecordFields FormatConfig.Default |> Array.map fst
let private (|FantomasSetting|_|) (s:string) =
    let s = s.Trim('\"')
    Array.tryFind ((=) s) fantomasFields

let private (|Number|_|) d =
    match System.Int32.TryParse(d) with
    | true, d -> Some (box d)
    | _ -> None
let private (|Boolean|_|) b =
    if b = "true" then Some (box true)
    elif b = "false" then Some (box false)
    else None

let processSetting originalLine key value =
    match (key, value) with
    | (FantomasSetting(fs), Number(v))
    | (FantomasSetting(fs), Boolean(v)) -> Ok (fs, v)
    | _ ->
        let warning = sprintf "%s is no valid setting for Fantomas v%s" originalLine (fantomasVersion.Value)
        Error warning