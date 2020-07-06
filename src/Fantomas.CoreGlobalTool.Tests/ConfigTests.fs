module Fantomas.CoreGlobalTool.Tests.ConfigTests

open NUnit.Framework
open FsUnit
open Fantomas.CoreGlobalTool.Tests.TestHelpers

[<Test>]
let ``config file in working directory should not require relative prefix, 821`` () =
    use fileFixture =
        new TemporaryFileCodeSample("let a  = // foo
                                                            9")

    use configFixture =
        new ConfigurationFile("""
[*.fs]
indent_size=2
"""                            )

    let (exitCode, output) =
        runFantomasTool fileFixture.Filename

    exitCode |> should equal 0
    output
    |> should startWith (sprintf "Processing %s" fileFixture.Filename)

    let result = System.IO.File.ReadAllText(fileFixture.Filename)
    result
    |> should equal """let a = // foo
  9
"""