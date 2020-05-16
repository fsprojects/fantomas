module Fantomas.CoreGlobalTool.Tests.ByteOrderMarkTests

open System.IO
open NUnit.Framework
open FsUnit
open Fantomas.CoreGlobalTool.Tests.TestHelpers
open System.Text

[<Literal>]
let Source = "namespace Company.Product.Feature"

let private getInitialBytes file =
        use file = new FileStream(file, FileMode.Open, FileAccess.Read)
        let mutable bom = Array.zeroCreate 3
        file.Read(bom, 0, 3) |> ignore
        bom

[<Test>]
let ``byte-order mark should be preserved, 795``() =
    use fileFixture = new TemporaryFileCodeSample(Source, true)
    let (exitCode,_) = runFantomasTool fileFixture.Filename
    exitCode |> should equal 0

    let expectedPreamble = Encoding.UTF8.GetPreamble()
    let actualPreamble = getInitialBytes fileFixture.Filename
    expectedPreamble |> should equal actualPreamble

[<Test>]
let ``preserve byte-order from original file`` () =
    use inputFixture = new TemporaryFileCodeSample(Source, true)
    use outputFixture = new OutputFile()
    let (exitCode,_) =
        sprintf "--out %s %s" outputFixture.Filename inputFixture.Filename
        |> runFantomasTool

    exitCode |> should equal 0

    let expectedPreamble = Encoding.UTF8.GetPreamble()
    let actualPreamble = getInitialBytes outputFixture.Filename
    expectedPreamble |> should equal actualPreamble
