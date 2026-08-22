module Fantomas.Tests.Integration.SmokeTests

open System.IO
open System.Text
open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelpers

// What is left of the tests that start a real fantomas process.
//
// Everything the tool decides is covered in memory, against a MockFileSystem, in the test files
// beside this folder. What that cannot reach is the wiring: that the arguments are parsed at all,
// that an exit code makes it out to the operating system, that a working directory and a real file
// on a real disk behave the way the mock says they do. That is what is left here, one case per
// thing, rather than a case per behaviour.

[<Literal>]
let private NeedsFormatting = "let  a =   1"

[<Test>]
let ``the help page is written and the process ends well`` () =
    let { ExitCode = exitCode; Output = output } = runFantomasTool [ "--help" ]

    exitCode |> should equal 0
    output |> should contain "Usage:"

[<Test>]
let ``a file named on the command line is formatted where it lies`` () =
    use fileFixture = new TemporaryFileCodeSample(NeedsFormatting)

    let { ExitCode = exitCode; Output = output } = formatCode [ fileFixture.Filename ]

    exitCode |> should equal 0
    output |> should contain "was formatted"
    File.ReadAllText fileFixture.Filename |> should equal "let a = 1\n"

[<Test>]
let ``an out path is written to and the input is left alone`` () =
    use inputFixture = new TemporaryFileCodeSample(NeedsFormatting)
    use outputFixture = new OutputFile()

    let { ExitCode = exitCode } =
        runFantomasTool [ "--out"; outputFixture.Filename; inputFixture.Filename ]

    exitCode |> should equal 0
    File.ReadAllText inputFixture.Filename |> should equal NeedsFormatting
    File.ReadAllText outputFixture.Filename |> should equal "let a = 1\n"

// 0, 99 and 1 are what a build script branches on, so each has to reach the operating system.
[<Test>]
let ``a check that finds nothing to do exits 0`` () =
    use fileFixture = new TemporaryFileCodeSample("let a = 1\n")

    let { ExitCode = exitCode } = checkCode [ fileFixture.Filename ]
    exitCode |> should equal 0

[<Test>]
let ``a check that finds a file needing formatting exits 99`` () =
    use fileFixture = new TemporaryFileCodeSample(NeedsFormatting)

    let { ExitCode = exitCode } = checkCode [ fileFixture.Filename ]
    exitCode |> should equal 99

[<Test>]
let ``an input path that cannot be used exits 1`` () =
    let { ExitCode = exitCode } = formatCode [ "somenonexistingfile.fs" ]
    exitCode |> should equal 1

[<Test>]
let ``a file that cannot be parsed exits 1`` () =
    use fileFixture = new TemporaryFileCodeSample("let a =")

    let { ExitCode = exitCode } = formatCode [ fileFixture.Filename ]
    exitCode |> should equal 1

// The in-memory tests take a MockFileSystem at its word about encodings. This says a real disk
// agrees with it.
[<Test>]
let ``a byte order mark survives a round trip through a real file`` () =
    use fileFixture =
        new TemporaryFileCodeSample(NeedsFormatting, hasByteOrderMark = true)

    let { ExitCode = exitCode } = formatCode [ fileFixture.Filename ]
    exitCode |> should equal 0

    let preamble = Encoding.UTF8.GetPreamble()

    use stream = new FileStream(fileFixture.Filename, FileMode.Open, FileAccess.Read)
    let actual = Array.zeroCreate preamble.Length
    stream.ReadExactly(actual, 0, preamble.Length)

    actual |> should equal preamble

// Reading the ignore file happens before either command runs, so a pattern the ignore library
// will not compile used to escape every handler and end the process with a stack trace.
[<Test>]
let ``a fantomasignore that cannot be read is reported rather than thrown`` () =
    use fileFixture = new TemporaryFileCodeSample(NeedsFormatting)
    use ignoreFixture = new FantomasIgnoreFile("a[")

    let { ExitCode = exitCode; Error = error } = formatCode [ fileFixture.Filename ]

    exitCode |> should equal 1
    Assert.That(error, Does.Not.Contain "Unhandled exception")
    Assert.That(error, Does.Not.Contain "   at ")

// The ignore file is found relative to the directory the tool was started in, which is the one
// thing about it a mock cannot stand in for.
[<Test>]
let ``a fantomasignore in the working directory is honoured`` () =
    use fileFixture = new TemporaryFileCodeSample(NeedsFormatting, fileName = "Ignored")
    use ignoreFixture = new FantomasIgnoreFile("Ignored.fs")

    let { ExitCode = exitCode; Output = output } = formatCode [ fileFixture.Filename ]

    exitCode |> should equal 0
    output |> should contain "was ignored"
    File.ReadAllText fileFixture.Filename |> should equal NeedsFormatting
