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
        new ConfiguredCodeSample(
            """
[*.fs]
indent_size=2
end_of_line=lf
""",
            "let a  = // foo
                                                            9"
        )

    let args = DetailedVerbosity @ [ fileFixture.Filename ]
    let { ExitCode = exitCode; Output = output } = runFantomasTool args
    exitCode |> should equal 0
    output |> should contain (sprintf "Processing %s" fileFixture.Filename)
    let result = System.IO.File.ReadAllText(fileFixture.Filename)

    result |> should equal "let a = // foo\n  9\n"

[<Test>]
let ``end_of_line=cr should throw an exception`` () =
    use fileFixture =
        new ConfiguredCodeSample(
            """
[*.fs]
end_of_line=cr
""",
            "let a = 9\n"
        )

    let args = DetailedVerbosity @ [ fileFixture.Filename ]
    let { ExitCode = exitCode; Error = error } = runFantomasTool args
    exitCode |> should equal 1
    Assert.That(error, Does.Contain "Carriage returns are not valid for F# code, please use one of 'lf' or 'crlf'")

let valid_eol_settings = [ "lf"; "crlf" ]

[<TestCaseSource("valid_eol_settings")>]
let ``uses end_of_line setting to write user newlines`` setting =
    let newline =
        match EndOfLineStyle.OfConfigString setting with
        | Some nl -> nl.NewLineString
        | None -> failwith $"unable to get %s{nameof EndOfLineStyle.OfConfigString}"

    let sampleCode nln =
        sprintf "let a = 9%s%slet b = 7%s" nln nln nln

    use fileFixture =
        new ConfiguredCodeSample(
            sprintf
                """
[*.fs]
end_of_line = %s
"""
                setting,
            sampleCode "\n"
        )

    let { ExitCode = exitCode } = runFantomasTool [ fileFixture.Filename ]
    exitCode |> should equal 0

    let result = System.IO.File.ReadAllText(fileFixture.Filename)

    let expected = sampleCode newline

    result |> should equal expected

[<Test>]
let ``end_of_line should be respected for ifdef`` () =
    let source = "#if FOO\n()\n#else\n()\n#endif"

    use fileFixture =
        new ConfiguredCodeSample(
            """
[*.fs]
end_of_line = lf
""",
            source
        )

    let { ExitCode = exitCode } = runFantomasTool [ fileFixture.Filename ]
    exitCode |> should equal 0

    let result = System.IO.File.ReadAllText(fileFixture.Filename)

    result |> should equal "#if FOO\n()\n#else\n()\n#endif\n"

// `EditorConfigReportTests` covers what a report says and when it is written. This is the one
// test that pays for a process: that the report reaches standard error rather than standard out,
// which is `Logging.createLogger`'s doing and not visible from inside the test host.
[<Test>]
let ``settings Fantomas cannot use are reported on standard error`` () =
    use fileFixture =
        new ConfiguredCodeSample(
            """
[*.fs]
fsharp_bogus_option = true
fsharp_experimental_elmish = not_a_bool
some_other_tool_setting = 42
""",
            "let a = 9\n"
        )

    let { ExitCode = exitCode
          Output = output
          Error = error } =
        formatCode [ fileFixture.Filename ]

    // Advice, not a failure: the file is still formatted, with defaults for what could not be read.
    exitCode |> should equal 0
    Assert.That(error, Does.Contain "'fsharp_bogus_option' is not a Fantomas setting")

    Assert.That(
        error,
        Does.Contain
            "'fsharp_experimental_elmish' does not accept the value 'not_a_bool', so the default is used instead"
    )

    // A setting without the fsharp_ prefix belongs to some other tool and is none of our business.
    Assert.That(error, Does.Not.Contain "some_other_tool_setting")
    Assert.That(output, Does.Not.Contain "fsharp_bogus_option")

[<Test>]
let ``an editorconfig Fantomas can act on reports nothing`` () =
    use fileFixture =
        new ConfiguredCodeSample(
            """
[*.fs]
max_line_length = 100
fsharp_multiline_bracket_style = stroustrup
indent_style = space
""",
            "let a = 9\n"
        )

    let { ExitCode = exitCode; Error = error } = formatCode [ fileFixture.Filename ]

    exitCode |> should equal 0
    error |> should equal ""
