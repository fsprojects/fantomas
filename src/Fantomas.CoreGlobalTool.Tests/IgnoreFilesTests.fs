module Fantomas.CoreGlobalTool.Tests.IgnoreFilesTests

open System.IO
open NUnit.Framework
open FsUnit
open Fantomas.CoreGlobalTool.Tests.TestHelpers

[<Literal>]
let Source = "let  foo =   47"

[<Test>]
let ``ignore all fs files``() =
    let fileName = "ToBeIgnored"
    use inputFixture = new TemporaryFileCodeSample(Source, fileName = fileName)
    use ignoreFixture = new FantomasIgnoreFile("*.fs")
    use outputFixture = new OutputFile()

    let (exitCode, _) =
        sprintf "--out %s %s" outputFixture.Filename inputFixture.Filename
        |> runFantomasTool

    exitCode |> should equal 0

    File.Exists outputFixture.Filename
    |> should equal false

[<Test>]
let ``ignore specific file`` () =
    let fileName = "A"
    use inputFixture = new TemporaryFileCodeSample(Source, fileName = fileName)
    use ignoreFixture = new FantomasIgnoreFile("A.fs")

    let (exitCode, output) = runFantomasTool inputFixture.Filename
    exitCode |> should equal 0

    output
    |> should contain "was ignored"

[<Test>]
let ``don't ignore other files`` () =
    let fileName = "B"
    use inputFixture = new TemporaryFileCodeSample(Source, fileName = fileName)
    use ignoreFixture = new FantomasIgnoreFile("A.fs")

    let (exitCode, output) = runFantomasTool inputFixture.Filename
    exitCode |> should equal 0

    output
    |> should contain "Processing"

    output
    |> should contain "B.fs"

[<Test>]
let ``ignore file in folder`` () =
    let fileName = "A"
    let subFolder = System.Guid.NewGuid().ToString("N")
    use inputFixture = new TemporaryFileCodeSample(Source, fileName = fileName, subFolder = subFolder)
    use ignoreFixture = new FantomasIgnoreFile("A.fs")

    let (exitCode, _) = runFantomasTool (sprintf ".%c%s"  Path.DirectorySeparatorChar subFolder)
    exitCode |> should equal 0

    File.ReadAllText inputFixture.Filename
    |> should equal Source

[<Test>]
let ``ignore file while checking`` () =
    let fileName = "A"
    use inputFixture = new TemporaryFileCodeSample(Source, fileName = fileName)
    use ignoreFixture = new FantomasIgnoreFile("A.fs")

    let (exitCode, output) =
        sprintf "%s --check" inputFixture.Filename
        |> runFantomasTool

    exitCode |> should equal 0

    output
    |> should contain "was ignored"

[<Test>]
let ``ignore file in folder while checking`` () =
    let fileName = "A"
    let subFolder = System.Guid.NewGuid().ToString("N")
    use inputFixture = new TemporaryFileCodeSample(Source, fileName = fileName, subFolder = subFolder)
    use ignoreFixture = new FantomasIgnoreFile("A.fs")

    let (exitCode, _) = runFantomasTool (sprintf ".%c%s --check"  Path.DirectorySeparatorChar subFolder)
    exitCode |> should equal 0

    File.ReadAllText inputFixture.Filename
    |> should equal Source