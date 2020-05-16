module Fantomas.CoreGlobalTool.Tests.ConfigTests

open NUnit.Framework
open FsUnit
open Fantomas.CoreGlobalTool.Tests.TestHelpers
open Fantomas.FormatConfig

[<Test>]
let ``config file in working directory should not require relative prefix, 821`` () =
    use fileFixture =
        new TemporaryFileCodeSample("let a  =  9")

    use configFixture =
        new ConfigurationFile({ FormatConfig.Default with
                                    IndentSpaceNum = 2 })

    let (exitCode, output) =
        runFantomasTool (sprintf "--config fantomas-config.json %s" fileFixture.Filename)

    exitCode |> should equal 0
    output
    |> should startWith (sprintf "Processing %s" fileFixture.Filename)