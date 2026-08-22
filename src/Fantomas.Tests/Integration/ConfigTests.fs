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

[<Literal>]
let private BadEditorConfig =
    """
[*.fs]
fsharp_bogus_option = true
fsharp_experimental_elmish = not_a_bool
some_other_tool_setting = 42
"""

let private occurrences (needle: string) (haystack: string) : int =
    haystack.Split([| needle |], System.StringSplitOptions.None).Length - 1

[<Test>]
let ``settings Fantomas cannot use are reported on standard error`` () =
    use fileFixture = new TemporaryFileCodeSample("let a = 9\n")
    use configFixture = new ConfigurationFile(BadEditorConfig)

    let { ExitCode = exitCode
          Output = output
          Error = error } =
        formatCode [ fileFixture.Filename ]

    exitCode |> should equal 0
    Assert.That(error, Does.Contain "fsharp_bogus_option is not a Fantomas setting")

    Assert.That(
        error,
        Does.Contain "fsharp_experimental_elmish does not accept the value not_a_bool, using the default instead"
    )

    Assert.That(error, Does.Contain "supports these .editorconfig settings:")
    Assert.That(error, Does.Contain "fsharp_multiline_bracket_style")
    // A setting without the fsharp_ prefix belongs to some other tool and is none of our business.
    Assert.That(error, Does.Not.Contain "some_other_tool_setting")
    Assert.That(output, Does.Not.Contain "fsharp_bogus_option")

[<Test>]
let ``settings Fantomas cannot use are reported once, however many files are formatted`` () =
    use firstFile = new TemporaryFileCodeSample("let a = 9\n")
    use secondFile = new TemporaryFileCodeSample("let b = 9\n")
    use thirdFile = new TemporaryFileCodeSample("let c = 9\n")
    use configFixture = new ConfigurationFile(BadEditorConfig)

    let { ExitCode = exitCode; Error = error } =
        formatCode [ firstFile.Filename; secondFile.Filename; thirdFile.Filename ]

    exitCode |> should equal 0

    occurrences "fsharp_bogus_option is not a Fantomas setting" error
    |> should equal 1

    occurrences "supports these .editorconfig settings:" error |> should equal 1

// Files are formatted in parallel, so the report has to leave one thread as a single message.
// Written as two, the settings list can overtake the problems it belongs to.
[<Test>]
let ``the problems come before the settings list, however many files are formatted`` () =
    use firstFile = new TemporaryFileCodeSample("let a = 9\n")
    use secondFile = new TemporaryFileCodeSample("let b = 9\n")
    use thirdFile = new TemporaryFileCodeSample("let c = 9\n")
    use fourthFile = new TemporaryFileCodeSample("let d = 9\n")
    use configFixture = new ConfigurationFile(BadEditorConfig)

    let { ExitCode = exitCode; Error = error } =
        formatCode
            [ firstFile.Filename
              secondFile.Filename
              thirdFile.Filename
              fourthFile.Filename ]

    exitCode |> should equal 0

    let problemsAt =
        error.IndexOf("Fantomas cannot use some settings from", System.StringComparison.Ordinal)

    let settingsAt =
        error.IndexOf("supports these .editorconfig settings:", System.StringComparison.Ordinal)

    Assert.That(problemsAt, Is.GreaterThanOrEqualTo 0)
    Assert.That(settingsAt, Is.GreaterThan problemsAt)

[<Test>]
let ``an editorconfig Fantomas can act on reports nothing`` () =
    use fileFixture = new TemporaryFileCodeSample("let a = 9\n")

    use configFixture =
        new ConfigurationFile(
            """
[*.fs]
max_line_length = 100
fsharp_multiline_bracket_style = stroustrup
indent_style = space
"""
        )

    let { ExitCode = exitCode; Error = error } = formatCode [ fileFixture.Filename ]

    exitCode |> should equal 0
    error |> should equal ""
