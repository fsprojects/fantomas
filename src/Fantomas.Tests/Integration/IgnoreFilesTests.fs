module Fantomas.Tests.Integration.IgnoreFilesTests

open System.IO
open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelpers

[<Literal>]
let Source = "let  foo =   47"

[<Literal>]
let Verbosity = "--verbosity d"

[<Test>]
let ``ignore all fs files`` () =
    let fileName = "ToBeIgnored"

    use inputFixture = new TemporaryFileCodeSample(Source, fileName = fileName)

    use ignoreFixture = new FantomasIgnoreFile("*.fs")
    use outputFixture = new OutputFile()

    let { ExitCode = exitCode } =
        sprintf "--out %s %s" outputFixture.Filename inputFixture.Filename
        |> runFantomasTool

    exitCode |> should equal 0
    File.Exists outputFixture.Filename |> should equal false

[<Test>]
let ``ignore specific file`` () =
    let fileName = "A"

    use inputFixture = new TemporaryFileCodeSample(Source, fileName = fileName)

    use ignoreFixture = new FantomasIgnoreFile("A.fs")
    let args = sprintf "%s %s" Verbosity inputFixture.Filename
    let { ExitCode = exitCode; Output = output } = runFantomasTool args
    exitCode |> should equal 0

    output |> should contain "was ignored"

[<Test>]
let ``ignore specific file in subfolder`` () =
    let fileName = "A"
    let sub1 = System.Guid.NewGuid().ToString("N")
    let sub2 = System.Guid.NewGuid().ToString("N")
    let subFolders = [| sub1; sub2 |]

    use inputFixture =
        new TemporaryFileCodeSample(Source, fileName = fileName, subFolders = subFolders)

    use ignoreFixture = new FantomasIgnoreFile(sprintf "%s/%s/A.fs" sub1 sub2)

    let { ExitCode = exitCode } =
        runFantomasTool (sprintf "--recurse --check .%c%s" Path.DirectorySeparatorChar sub1)

    exitCode |> should equal 0

[<Test>]
let ``don't ignore other files`` () =
    let fileName = "B"

    use inputFixture = new TemporaryFileCodeSample(Source, fileName = fileName)

    use ignoreFixture = new FantomasIgnoreFile("A.fs")
    let args = sprintf "%s %s" Verbosity inputFixture.Filename
    let { ExitCode = exitCode; Output = output } = runFantomasTool args
    exitCode |> should equal 0

    output |> should contain "Processing"

    output |> should contain "B.fs"

[<Test>]
let ``ignore file in folder`` () =
    let fileName = "A"
    let subFolder = System.Guid.NewGuid().ToString("N")

    use inputFixture =
        new TemporaryFileCodeSample(Source, fileName = fileName, subFolder = subFolder)

    use ignoreFixture = new FantomasIgnoreFile("A.fs")

    let { ExitCode = exitCode; Output = output } =
        runFantomasTool (sprintf "%s .%c%s" Verbosity Path.DirectorySeparatorChar subFolder)

    exitCode |> should equal 0
    File.ReadAllText inputFixture.Filename |> should equal Source

    output |> should contain "│ 0 │ Ignored │ 0 │ Unchanged │ 0 │ Errored │ 0"

[<Test>]
let ``ignore file while checking`` () =
    let fileName = "A"

    use inputFixture = new TemporaryFileCodeSample(Source, fileName = fileName)

    use ignoreFixture = new FantomasIgnoreFile("A.fs")

    let { ExitCode = exitCode; Output = output } =
        sprintf "%s %s --check" Verbosity inputFixture.Filename |> runFantomasTool

    exitCode |> should equal 0

    output |> should contain "was ignored"

[<Test>]
let ``ignore file in folder while checking`` () =
    let fileName = "A"
    let subFolder = System.Guid.NewGuid().ToString("N")

    use inputFixture =
        new TemporaryFileCodeSample(Source, fileName = fileName, subFolder = subFolder)

    use ignoreFixture = new FantomasIgnoreFile("A.fs")

    let { ExitCode = exitCode } =
        runFantomasTool (sprintf ".%c%s --check" Path.DirectorySeparatorChar subFolder)

    exitCode |> should equal 0
    File.ReadAllText inputFixture.Filename |> should equal Source

[<Test>]
let ``honor ignore file when processing a folder`` () =
    let fileName = "A"
    let subFolder = System.Guid.NewGuid().ToString("N")

    use ignoreFixture =
        new TemporaryFileCodeSample(Source, fileName = fileName, subFolder = subFolder)

    use inputFixture = new FantomasIgnoreFile("*.fsx")

    let { Output = output } =
        runFantomasTool (sprintf "%s .%c%s" Verbosity Path.DirectorySeparatorChar subFolder)

    output |> should not' (contain "ignored")

    output
    |> should contain "Formatted │ 1 │ Ignored │ 0 │ Unchanged │ 0 │ Errored │ 0"
