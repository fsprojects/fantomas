module Fantomas.Tests.FantomasConfigTests

open Fantomas
open Fantomas.FormatConfig
open Fantomas.FantomasConfig
open NUnit.Framework
open System.IO
open TestHelper

[<Test>]
let tryLoadShouldErrorWhenFileIsMissing() =
    let path = getFile "tryLoadShouldErrorWhenFileIsMissing.json"
    match tryLoad path with
    | Ok x -> failwithf "Expected Error, but got Ok %A" x
    | Error e -> StringAssert.Contains(path, e)

[<Test>]
let loadShouldThrowWhenFileIsMissing() =
    let path = getFile "loadShouldThrowWhenFileIsMissing.json"
    let ex =  Assert.Throws(fun _ -> load path |> ignore)
    StringAssert.Contains(path, ex.Message)

[<Test>]
let loadShouldWorkSimple() =
    let json = """{ "IndentSpaceNum" : 1 }"""
    let path = getFile "loadShouldWorkSimple.json"
    File.WriteAllText(path, json)
    
    let actual = load(path)
    let expected = 
        { Warnings = [||]
          FileName = path
          FantomasConfig = { Dto.defaultConfig() with IndentSpaceNum = 1 }
          FormatConfig = { FormatConfig.Default with IndentSpaceNum = 1 } }

    Assert.AreEqual(expected, actual)

[<Test>]
let loadShouldWarnOnMissingProperty() =
    let json = """{ "ThisPropertyDoesNotExist" : 1 }"""
    let path = getFile "loadShouldThrowOnMissingProperty.json"
    File.WriteAllText(path, json)

    let cfg = load(path)
    Assert.AreEqual(1, cfg.Warnings.Length)
    StringAssert.Contains("ThisPropertyDoesNotExist", cfg.Warnings.[0])

[<Test>]
let tryFindConfigShouldReturnNone() =
    let dir = getFile "."
    Assert.AreEqual(None, tryFindConfig dir)

[<Test>]
let tryFindConfigShouldReturnSome() =
    let mkDir d = if d |> Directory.Exists |> not then d |> Directory.CreateDirectory |> ignore

    let fileName = getFile "./tryFindConfigShouldReturnSome/.fantomas-config"
    mkDir (getFile "./tryFindConfigShouldReturnSome")
    mkDir (getFile "./tryFindConfigShouldReturnSome/Level1")
    mkDir (getFile "./v/Level1/Level2")

    let json = "{}"
    File.WriteAllText(fileName, json)
    
    Assert.AreNotEqual(None, tryFindConfig(getFile "./tryFindConfigShouldReturnSome"))
    Assert.AreNotEqual(None, tryFindConfig(getFile "./tryFindConfigShouldReturnSome/Level1"))
    Assert.AreNotEqual(None, tryFindConfig(getFile "./tryFindConfigShouldReturnSome/Level2"))

[<Test>]
let tryFindAndLoadConfigShouldWork() =
    let mkDir d = if d |> Directory.Exists |> not then d |> Directory.CreateDirectory |> ignore

    let fileName = getFile "./tryFindAndLoadConfigShouldWork/.fantomas-config"
    mkDir (getFile "./tryFindAndLoadConfigShouldWork")
    mkDir (getFile "./tryFindAndLoadConfigShouldWork/Level1")
    mkDir (getFile "./tryFindAndLoadConfigShouldWork/Level1/Level2")

    let json = "{}"
    File.WriteAllText(fileName, json)

    let checkDir f =
        match tryFindAndLoadConfig(getFile f) with
        | Some (Ok _) -> ()
        | otherwise -> failwithf "Expected Some(Ok), got: %A" otherwise

    checkDir "./tryFindAndLoadConfigShouldWork"
    checkDir "./tryFindAndLoadConfigShouldWork/Level1"
    checkDir "./tryFindAndLoadConfigShouldWork/Level2"


[<Test>]
let tryFindAndLoadConfigShouldNotCrash() =
    let mkDir d = if d |> Directory.Exists |> not then d |> Directory.CreateDirectory |> ignore

    let fileName = getFile "./tryFindAndLoadConfigShouldNotCrash/.fantomas-config"
    mkDir (getFile "./tryFindAndLoadConfigShouldNotCrash")
    mkDir (getFile "./tryFindAndLoadConfigShouldNotCrash/Level1")
    mkDir (getFile "./tryFindAndLoadConfigShouldNotCrash/Level1/Level2")

    let json = "this is invalid json"
    File.WriteAllText(fileName, json)

    let checkDir f =
        match tryFindAndLoadConfig(getFile f) with
        | Some (Error _) -> ()
        | otherwise -> failwithf "Expected Some(Error), got: %A" otherwise

    checkDir "./tryFindAndLoadConfigShouldNotCrash"
    checkDir "./tryFindAndLoadConfigShouldNotCrash/Level1"
    checkDir "./tryFindAndLoadConfigShouldNotCrash/Level2"
