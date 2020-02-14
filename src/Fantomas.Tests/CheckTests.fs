module Fantomas.Tests.CheckTests

open NUnit.Framework
open FsUnit
open Fantomas.FormatConfig
open Fantomas.FakeHelpers
open Fantomas.Tests.TestHelper

let private config = FormatConfig.Default

[<Literal>]
let NeedsFormatting = """module A

let a =       5
let b= a +      123
"""

[<Literal>]
let WithErrors = """le a 2"""

[<Literal>]
let CorrectlyFormated = """module A

"""

[<Test>]
let ``formatted files should report no changes``() =
    use fileFixture = new TemporaryFileCodeSample(CorrectlyFormated)

    let result =
        fileFixture.Filename
        |> Seq.singleton
        |> checkCode config
        |> Async.RunSynchronously

    result.NeedsFormatting |> should equal false
    result.IsValid |> should equal true

[<Test>]
let ``files with errors should report an internal error``() =
    use fileFixture = new TemporaryFileCodeSample(WithErrors)

    let result =
        fileFixture.Filename
        |> Seq.singleton
        |> checkCode config
        |> Async.RunSynchronously

    result.HasErrors |> should equal true
    List.length result.Errors |> should equal 1

[<Test>]
let ``files that need formatting should report that they need to be formatted``() =
    use fileFixture = new TemporaryFileCodeSample(NeedsFormatting)

    let result =
        fileFixture.Filename
        |> Seq.singleton
        |> checkCode config
        |> Async.RunSynchronously

    result.HasErrors |> should equal false
    result.NeedsFormatting |> should equal true
    List.length result.Formatted |> should equal 1
