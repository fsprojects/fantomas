module Fantomas.Tests.Integration.HelpPageTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelpers

// What the page says is settled in HelpPageTests, against `HelpPage.render`. What is left here is
// what only a real process shows: which stream the page goes to, and how much colour the terminal
// it is attached to actually gets.

[<Test>]
[<TestCase("--help")>]
[<TestCase("-h")>]
let ``both spellings of the flag write the page to standard out`` (flag: string) =
    let {
            ExitCode = exitCode
            Output = output
            Error = error
        } =
        runFantomasTool [ flag ]

    exitCode |> should equal 0
    error |> should equal ""
    Assert.That(output, Does.Contain "Usage: fantomas [command] [...flags] [...paths]")

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

    Assert.That(output, Does.Contain "Usage: fantomas [command] [...flags] [...paths]")
    output.Contains "\u001b[" |> should equal false

[<Test>]
let ``an argument error is reported on standard error, with a pointer to the page`` () =
    let {
            ExitCode = exitCode
            Output = output
            Error = error
        } =
        runFantomasTool [ "--out" ]

    exitCode |> should not' (equal 0)
    output |> should equal ""
    Assert.That(error, Does.Contain "'--out' must be followed by a value.")
    Assert.That(error, Does.Contain "--help for usage information.")
    // Argu used to append a usage block of its own, which the page above replaces.
    Assert.That(error, Does.Not.Contain "USAGE:")
