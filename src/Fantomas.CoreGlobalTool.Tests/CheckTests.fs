module Fantomas.CoreGlobalTool.Tests.CheckTests

open NUnit.Framework
open FsUnit
open Fantomas.CoreGlobalTool.Tests.TestHelpers

[<Literal>]
let NeedsFormatting = """module A

let a =       5
let b= a +      123
"""

[<Literal>]
let WithErrors = """let a ="""

[<Literal>]
let CorrectlyFormatted = """module A

"""

[<Test>]
let ``formatted files should report exit code 0`` () =
    use fileFixture =
        new TemporaryFileCodeSample(CorrectlyFormatted)

    let (exitCode, _) = checkCode fileFixture.Filename
    exitCode |> should equal 0

[<Test>]
let ``invalid files should report exit code 1 with check`` () =
    use fileFixture = new TemporaryFileCodeSample(WithErrors)
    let (exitCode, _) = checkCode fileFixture.Filename
    exitCode |> should equal 1

[<Test>]
let ``invalid files should report exit code 1`` () =
    use fileFixture = new TemporaryFileCodeSample(WithErrors)
    let (exitCode, _) = formatCode fileFixture.Filename
    exitCode |> should equal 1

[<Test>]
let ``files that need formatting should report exit code 99`` () =
    use fileFixture =
        new TemporaryFileCodeSample(NeedsFormatting)

    let (exitCode, _) = checkCode fileFixture.Filename
    exitCode |> should equal 99

[<Test>]
let ``check with Program.fs file`` () =
    let codeSnippet = """[<EntryPoint>]
let main _ = 0
"""

    use fileFixture =
        new TemporaryFileCodeSample(codeSnippet, fileName = "Program")

    let (exitCode, _) = checkCode fileFixture.Filename
    exitCode |> should equal 0

[<Test>]
let ``check with different line endings`` () =
    let codeSnippet = """let a =
    // some comment
    42
"""

    let snippetWithOtherLineEndings =
        if codeSnippet.Contains("\r\n") then
            codeSnippet.Replace("\r\n", "\n")
        else
            codeSnippet.Replace("\n", "\r\n")

    use fileFixture =
        new TemporaryFileCodeSample(snippetWithOtherLineEndings)

    let (exitCode, _) = checkCode fileFixture.Filename
    exitCode |> should equal 0
