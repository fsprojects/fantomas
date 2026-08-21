module Fantomas.Tests.Integration.HelpPageTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelpers

[<Test>]
[<TestCase("--help")>]
[<TestCase("-h")>]
let ``help page is written to standard out`` (flag: string) =
    let { ExitCode = exitCode
          Output = output
          Error = error } =
        runFantomasTool [ flag ]

    exitCode |> should equal 0
    error |> should equal ""
    Assert.That(output, Does.Contain "Usage: fantomas [...flags] [...paths]")

[<Test>]
let ``help page lists every flag`` () =
    let { Output = output } = runFantomasTool [ "--help" ]

    for flag in
        [ "--check"
          "--out"
          "--force"
          "--profile"
          "--daemon"
          "--verbosity"
          "--version"
          "--help" ] do
        Assert.That(output, Does.Contain flag)

[<Test>]
let ``help page carries the version`` () =
    let { Output = output } = runFantomasTool [ "--help" ]
    let version = Fantomas.Core.CodeFormatter.GetVersion()
    let versionNumber = version.Split('+').[0]

    Assert.That(output, Does.Contain versionNumber)

[<Test>]
let ``help page links the documentation, the Discord and the llms files`` () =
    let { Output = output } = runFantomasTool [ "--help" ]

    for link in
        [ "https://fsprojects.github.io/fantomas/docs"
          "https://discord.com/channels/196693847965696000/1493226271767924747"
          "https://fsprojects.github.io/fantomas/llms.txt"
          "https://fsprojects.github.io/fantomas/llms-full.txt" ] do
        Assert.That(output, Does.Contain link)

// Standard out is redirected here, so the page has to come back as plain text.
[<Test>]
let ``help page is not coloured when standard out is redirected`` () =
    let { Output = output } = runFantomasTool [ "--help" ]

    output.Contains "\u001b[" |> should equal false

// Spectre.Console reports that ANSI is supported once it detects a CI environment, even with
// standard out redirected, because a CI log viewer renders escape codes. A help page that a
// build step captures still has to be plain, so redirection has to win over that.
[<Test>]
[<TestCase("GITHUB_ACTIONS")>]
[<TestCase("TF_BUILD")>]
let ``help page is not coloured on a build agent`` (variable: string) =
    let { Output = output } =
        runFantomasToolWithEnvironment [ variable, "true"; "TERM", "xterm-256color" ] [ "--help" ]

    Assert.That(output, Does.Contain "Usage: fantomas [...flags] [...paths]")
    output.Contains "\u001b[" |> should equal false

[<Test>]
let ``an argument error is reported on standard error without Argu's usage text`` () =
    let { ExitCode = exitCode
          Output = output
          Error = error } =
        runFantomasTool [ "--out" ]

    exitCode |> should not' (equal 0)
    output |> should equal ""
    Assert.That(error, Does.Contain "argument '--out' must be followed by")
    Assert.That(error, Does.Contain "Run fantomas --help for usage information.")
    Assert.That(error, Does.Not.Contain "USAGE:")
