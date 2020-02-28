module Fantomas.Tests.FormatConfigJsonConfigurationFileTests

open System
open Fantomas
open Fantomas.FormatConfig
open NUnit.Framework
open System.IO
open Fantomas.Tests.TestHelper

[<Literal>]
let private configFileName ="fantomas-config.json"

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

let private mkConfig subFolder config =
    let json = configToJson config
    mkConfigFromContent configFileName subFolder json

let private mkConfigFromJson subFolder json =
    mkConfigFromContent configFileName subFolder json

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

let private applyOptionsToConfig config path =
    let json = File.ReadAllText path
    let options, warnings = JsonConfig.parseOptionsFromJson json
    FormatConfig.applyOptions(config, options),warnings

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
let ``pointing to subfolder containing dot and trailing slash returns parent config file`` () =
    let parentFile = mkConfig None FormatConfig.Default
    let subDir = uniqueString() + ".dir"
    mkConfigFromContent "Test.fs" (Some(subDir)) "" |> ignore
    let dirSep = string Path.DirectorySeparatorChar
    let childFolder = Path.GetDirectoryName(parentFile) + dirSep + subDir + dirSep
    try
        let paths = ConfigFile.findConfigurationFiles childFolder
        paths == [parentFile]
    finally
        delete parentFile
        delete childFolder

[<Test>]
let ``pointing to subfolder conntaining dot without trailing slash returns parent config file`` () =
    let parentFile = mkConfig None FormatConfig.Default
    let subDir = uniqueString() + ".dir"
    mkConfigFromContent "Test.fs" (Some(subDir)) "" |> ignore
    let dirSep = string Path.DirectorySeparatorChar
    let childFolder = Path.GetDirectoryName(parentFile) + dirSep + subDir
    try
        let paths = ConfigFile.findConfigurationFiles childFolder
        paths == [parentFile]
    finally
        delete parentFile
        delete childFolder

[<Test>]
let ``simple config file parses valid option`` () =
    let path = mkConfigFromJson None "{\"KeepNewlineAfter\":true}"
    let config, warnings = applyOptionsToConfig FormatConfig.Default path
    true == config.KeepNewlineAfter
    [] == warnings

[<Test>]
let ``keys should not necessarily have quotes to be parsed`` () =
    let path = mkConfigFromJson None "{KeepNewlineAfter:true}"
    let config, warnings = applyOptionsToConfig FormatConfig.Default path
    true == config.KeepNewlineAfter
    [] == warnings

[<Test>]
let ``invalid option returns a warning`` () =
    let path = mkConfigFromJson None "{ \"Foo\": true }"
    let config, warnings = applyOptionsToConfig FormatConfig.Default path
    config == FormatConfig.Default
    match warnings with
    | [| error |] ->
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
    let _, warnings =
        File.ReadAllText path
        |> JsonConfig.parseOptionsFromJson
    [] == warnings