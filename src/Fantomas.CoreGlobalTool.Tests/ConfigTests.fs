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

    let (exitCode, output) = runFantomasTool fileFixture.Filename

    exitCode |> should equal 0

    output
    |> should startWith (sprintf "Processing %s" fileFixture.Filename)

    let result =
        System.IO.File.ReadAllText(fileFixture.Filename)

    result
    |> should equal """let a = // foo
  9
"""

[<Test>]
let uses_end_of_line_settings_to_write_user_newlines () =
    let sampleCode = """let a = 9\n"""
    let codeWithNewline = sampleCode

    use fileFixture =
        new TemporaryFileCodeSample(codeWithNewline)

    use configFixture =
        new ConfigurationFile("""
[*.fs]
end_of_line = crlf
"""                            )

    let (exitCode, output) = runFantomasTool fileFixture.Filename

    exitCode |> should equal 0

    output
    |> should startWith (sprintf "Processing %s" fileFixture.Filename)

    let result =
        System.IO.File.ReadAllText(fileFixture.Filename)

    let expected = sampleCode.Replace("\n", "\r\n")
    //     .Split([| "\r\n"; "\n" |], System.StringSplitOptions.None) // maybe chomping some whitespace here?
    // |> String.concat "\r\n" // todo: user-specific string here
    // |> fun s -> s + "    \r\n"

    result |> should equal expected
