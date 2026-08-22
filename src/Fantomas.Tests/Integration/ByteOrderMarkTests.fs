module Fantomas.Tests.Integration.ByteOrderMarkTests

open System.IO
open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelpers
open System.Text

[<Literal>]
let Source = "namespace Company.Product.Feature"

let private getInitialBytes file =
    use file = new FileStream(file, FileMode.Open, FileAccess.Read)

    let mutable bom = Array.zeroCreate 3
    file.Read(bom, 0, 3) |> ignore
    bom

[<Test>]
let ``byte-order mark should be preserved, 795`` () =
    use fileFixture = new TemporaryFileCodeSample(Source, true)

    let { ExitCode = exitCode } = runFantomasTool [ fileFixture.Filename ]
    exitCode |> should equal 0

    let expectedPreamble = Encoding.UTF8.GetPreamble()
    let actualPreamble = getInitialBytes fileFixture.Filename
    expectedPreamble |> should equal actualPreamble

[<Test>]
let ``preserve byte-order from original file`` () =
    use inputFixture = new TemporaryFileCodeSample(Source, true)

    use outputFixture = new OutputFile()

    let { ExitCode = exitCode } =
        [ "--out"; outputFixture.Filename; inputFixture.Filename ] |> runFantomasTool

    exitCode |> should equal 0

    let expectedPreamble = Encoding.UTF8.GetPreamble()
    let actualPreamble = getInitialBytes outputFixture.Filename
    expectedPreamble |> should equal actualPreamble

[<Test>]
let ``file without byte-order mark does not gain one`` () =
    use inputFixture = new TemporaryFileCodeSample(Source)

    use outputFixture = new OutputFile()

    let { ExitCode = exitCode } =
        [ "--out"; outputFixture.Filename; inputFixture.Filename ] |> runFantomasTool

    exitCode |> should equal 0

    let preamble = Encoding.UTF8.GetPreamble()
    let actualPreamble = getInitialBytes outputFixture.Filename
    actualPreamble |> should not' (equal preamble)

[<Test>]
let ``content already at the output path is replaced, not written over`` () =
    use inputFixture = new TemporaryFileCodeSample(Source, true)

    use outputFixture = new OutputFile()

    // A byte-order mark used to mean the output file was opened without being truncated, so the
    // tail of whatever was there before survived a write of shorter content.
    File.WriteAllText(
        outputFixture.Filename,
        "// leftovers leftovers leftovers leftovers leftovers leftovers\n",
        Encoding.UTF8
    )

    let { ExitCode = exitCode } =
        [ "--out"; outputFixture.Filename; inputFixture.Filename ] |> runFantomasTool

    exitCode |> should equal 0

    File.ReadAllText outputFixture.Filename |> should not' (contain "leftovers")
