module Fantomas.Tests.Integration.ForceTests

open System.IO
open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelpers

[<Literal>]
let Verbosity = "--verbosity d"

// The day this test fails because Fantomas can format the file, is the day you can remove this file.

[<Test>]
let ``code that was invalid should be still be written`` () =
    let pwd = Path.GetDirectoryName(typeof<TemporaryFileCodeSample>.Assembly.Location)

    let sourceFile =
        Path.Combine(pwd, "..", "..", "..", "..", "tests", "data", "CheckDeclarations.fs")
        |> Path.GetFullPath

    if not (File.Exists sourceFile) then
        failwithf $"CheckDeclarations.fs was not found at \"%s{sourceFile}\""

    use outputFixture = new OutputFile()

    let { ExitCode = exitCode; Output = output } =
        runFantomasTool $"%s{Verbosity} --force --out %s{outputFixture.Filename} %s{sourceFile}"

    exitCode |> should equal 0
    output |> should contain "was not valid after formatting"
    output |> should contain "has been written"
    File.Exists outputFixture.Filename |> should equal true
