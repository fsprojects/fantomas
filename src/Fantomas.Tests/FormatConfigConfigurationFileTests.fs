module Fantomas.Tests.FormatConfigConfigurationFileTests

open System
open Fantomas
open Fantomas.FormatConfig
open NUnit.Framework
open System.IO
open Fantomas.Tests.TestHelper

let private getTempFolder () = Path.GetTempPath()

let private configToJson config =
    Reflection.getRecordFields config
    |> Array.choose (fun (k,v) ->
        match v with
        | :? System.Boolean as b ->
            sprintf "\"%s\":%s" k (if b then "true " else "false")
            |> Some
        | :? System.Int32 as i ->
            sprintf " \"%s\":%d" k i
            |> Some
        | _ -> None
    )
    |> String.concat ",\n  "
    |> sprintf "{ %s }"

let private mkConfigPath folder =
    match folder with
    | Some folder ->
        let folderPath = Path.Combine(getTempFolder(), folder)
        Directory.CreateDirectory(folderPath) |> ignore
        Path.Combine(folderPath, "fantomas-config.json")
    | None ->
        Path.Combine(getTempFolder(), "fantomas-config.json")
let private mkConfig folder fantomasConfig =
    let file = mkConfigPath folder
    let content = configToJson fantomasConfig
    File.WriteAllText(file, content)
    file

let private mkConfigFromJson folder json =
    let file = mkConfigPath folder
    File.WriteAllText(file, json)
    file

let rec private delete fileOrFolder =
    if File.Exists(fileOrFolder) then
        File.Delete fileOrFolder
    else if Directory.Exists fileOrFolder then
        Directory.EnumerateFiles fileOrFolder
        |> Seq.iter delete
        Directory.Delete fileOrFolder
    else
        ()
let private uniqueString () = System.Guid.NewGuid().ToString("N")


[<Test>]
let ``single configuration file`` () =
    let file = mkConfig None FormatConfig.Default
    try
        let paths = ConfigFile.findConfigurationFiles file
        paths == [file]
    finally
        delete file

[<Test>]
let ``folder with configuration file`` () =
    let file = mkConfig None FormatConfig.Default
    let folder = Path.GetDirectoryName(file)
    try
        let paths = ConfigFile.findConfigurationFiles folder
        paths == [file]
    finally
        delete file

[<Test>]
let ``pointing to subfolder should return parent config file as well`` () =
    let parentFile = mkConfig None FormatConfig.Default
    let childFile = mkConfig (Some(uniqueString())) FormatConfig.Default
    let childFolder = Path.GetDirectoryName(childFile)
    try
        let paths = ConfigFile.findConfigurationFiles childFolder
        paths == [parentFile; childFile]
    finally
        delete parentFile
        delete childFolder

[<Test>]
let ``pointing to config in a subfolder should return parent config file as well`` () =
    let parentFile = mkConfig None FormatConfig.Default
    let childFile = mkConfig (Some(uniqueString())) FormatConfig.Default
    let childFolder = Path.GetDirectoryName(childFile)
    try
        let paths = ConfigFile.findConfigurationFiles childFile
        paths == [parentFile; childFile]
    finally
        delete parentFile
        delete childFolder

[<Test>]
let ``simple config file parses valid option`` () =
    let path = mkConfigFromJson None "{\"KeepNewlineAfter\":true}"
    let config, warnings = ConfigFile.applyOptionsToConfig FormatConfig.Default path
    true == config.KeepNewlineAfter
    [] == warnings

[<Test>]
let ``keys should not necessarily have quotes to be parsed`` () =
    let path = mkConfigFromJson None "{KeepNewlineAfter:true}"
    let config, warnings = ConfigFile.applyOptionsToConfig FormatConfig.Default path
    true == config.KeepNewlineAfter
    [] == warnings

[<Test>]
let ``invalid option returns a warning`` () =
    let path = mkConfigFromJson None "{ \"Foo\": true }"
    let config, warnings = ConfigFile.applyOptionsToConfig FormatConfig.Default path
    config == FormatConfig.Default
    match warnings with
    | [ error ] ->
        StringAssert.IsMatch("\"Foo\":true is no valid setting for Fantomas v(.*)", error)
    | _ -> fail()

[<Test>]
let ``non existing file should return an error`` () =
    let path = Path.Combine(Path.GetTempPath(), uniqueString())
    let result = CodeFormatter.ReadConfiguration path
    match result with
    | Failure f ->
        (sprintf "No configuration files were found for %s" path) == f.Message
    | _ -> fail()

[<Test>]
let ``child configuration should overwrite parent folder`` () =
    let parentConfig = mkConfigFromJson None "{\"PageWidth\": 70}"
    let childConfig = mkConfigFromJson (Some(uniqueString())) "{\"PageWidth\": 90}"
    let childFolder = Path.GetDirectoryName(childConfig)

    try
        let result = CodeFormatter.ReadConfiguration childConfig
        match result with
        | Success config -> 90 == config.PageWidth
        | _ -> fail()
    finally
        delete parentConfig
        delete childFolder

[<Test>]
let ``invalid key in parent config should return partial success`` () =
    let parentConfig = mkConfigFromJson None "{\"PageWidthX\": 70}"
    let childConfig = mkConfigFromJson (Some(uniqueString())) "{\"PageWidth\": 130}"
    let childFolder = Path.GetDirectoryName(childConfig)

    try
        let result = CodeFormatter.ReadConfiguration childConfig
        match result with
        | PartialSuccess (config, [warning]) ->
            130 == config.PageWidth
            let pieces = warning.Split([|','|])
            StringAssert.Contains("\"PageWidthX\"", pieces.[0])
            StringAssert.Contains(parentConfig, pieces.[1])
        | _ -> fail()
    finally
        delete parentConfig
        delete childFolder

[<Test>]
let ``$schema key should not return warning`` () =
    let path = mkConfigFromJson None """
{
    "$schema": "http://json.schemastore.org/fantomas",
    "KeepNewlineAfter": true,
    "PageWidth": 99,
    "IndentSpaceNum": 2,
    "IndentOnTryWith": true
}
"""
    let _, warnings = ConfigFile.applyOptionsToConfig FormatConfig.Default path
    [] == warnings