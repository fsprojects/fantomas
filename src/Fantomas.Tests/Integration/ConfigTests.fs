module Fantomas.Tests.Integration.ConfigTests

open Fantomas.Core
open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelpers

let DetailedVerbosity = [ "--verbosity"; "d" ]

[<Literal>]
let NormalVerbosity = "--verbosity n"

[<Test>]
let ``config file in working directory should not require relative prefix, 821`` () =
    use fileFixture =
        new TemporaryFileCodeSample(
            "let a  = // foo
                                                            9"
        )

    use configFixture =
        new ConfigurationFile(
            """
[*.fs]
indent_size=2
end_of_line=lf
"""
        )

    let args = DetailedVerbosity @ [ fileFixture.Filename ]
    let { ExitCode = exitCode; Output = output } = runFantomasTool args
    exitCode |> should equal 0
    output |> should contain (sprintf "Processing %s" fileFixture.Filename)
    let result = System.IO.File.ReadAllText(fileFixture.Filename)

    result |> should equal "let a = // foo\n  9\n"

[<Test>]
let ``end_of_line=cr should throw an exception`` () =
    use fileFixture = new TemporaryFileCodeSample("let a = 9\n")

    use configFixture =
        new ConfigurationFile(
            """
[*.fs]
end_of_line=cr
"""
        )

    let args = DetailedVerbosity @ [ fileFixture.Filename ]
    let { ExitCode = exitCode; Output = output } = runFantomasTool args
    exitCode |> should equal 1
    Assert.That(output, Does.Contain "Carriage returns are not valid for F# code, please use one of 'lf' or 'crlf'")

let valid_eol_settings = [ "lf"; "crlf" ]

[<TestCaseSource("valid_eol_settings")>]
let ``uses end_of_line setting to write user newlines`` setting =
    let newline =
        match EndOfLineStyle.OfConfigString setting with
        | Some nl -> nl.NewLineString
        | None -> failwith $"unable to get %s{nameof EndOfLineStyle.OfConfigString}"

    let sampleCode nln =
        sprintf "let a = 9%s%slet b = 7%s" nln nln nln

    use fileFixture = new TemporaryFileCodeSample(sampleCode "\n")

    use configFixture =
        new ConfigurationFile(
            sprintf
                """
[*.fs]
end_of_line = %s
"""
                setting
        )

    let { ExitCode = exitCode } = runFantomasTool [ fileFixture.Filename ]
    exitCode |> should equal 0

    let result = System.IO.File.ReadAllText(fileFixture.Filename)

    let expected = sampleCode newline

    result |> should equal expected

[<Test>]
let ``end_of_line should be respected for ifdef`` () =
    let source = "#if FOO\n()\n#else\n()\n#endif"
    use fileFixture = new TemporaryFileCodeSample(source)

    use configFixture =
        new ConfigurationFile(
            """
[*.fs]
end_of_line = lf
"""
        )

    let { ExitCode = exitCode } = runFantomasTool [ fileFixture.Filename ]
    exitCode |> should equal 0

    let result = System.IO.File.ReadAllText(fileFixture.Filename)

    result |> should equal "#if FOO\n()\n#else\n()\n#endif\n"
