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

    let fileName = getFile "./Temp/.fantomas-config"
    mkDir (getFile "./Temp")
    mkDir (getFile "./Temp/Level1")
    mkDir (getFile "./Temp/Level1/Level2")

    let json = "{}"
    File.WriteAllText(fileName, json)
    
    Assert.AreNotEqual(None, tryFindConfig(getFile "./Temp"))
    Assert.AreNotEqual(None, tryFindConfig(getFile "./Temp/Level1"))
    Assert.AreNotEqual(None, tryFindConfig(getFile "./Temp/Level2"))
