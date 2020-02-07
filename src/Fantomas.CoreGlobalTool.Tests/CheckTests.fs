namespace Fantomas.CoreGlobalTool.Tests

module CheckTests =

    open System
    open System.IO
    open NUnit.Framework
    open FsUnit
    open Fantomas.FormatConfig
    open Fantomas.Cmd

    type TemporaryFileCodeSample internal (codeSnippet: string) =
        let filename = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString() + ".fs")
        do File.WriteAllText(filename, codeSnippet)

        member _.Filename: string = filename
        interface IDisposable with
            member this.Dispose(): unit = File.Delete(filename)

    [<Literal>]
    let NeedsFormatting = """module A

let a =       5
let b= a +      123
"""

    [<Literal>]
    let WithErrors = """le a 2"""

    [<Literal>]
    let CorrectlyFormated = """module A

"""

    [<Test>]
    let ``Formatted files should report no changes``() =
        use fileFixture = new TemporaryFileCodeSample(CorrectlyFormated)

        let config = FormatConfig.Default
        let output = new StringWriter()

        let exitCode =
            fileFixture.Filename
            |> Seq.singleton
            |> Check.run config output

        exitCode |> should equal Check.ExitCode.NoChanges

    [<Test>]
    let ``Files with errors should report an internal error``() =
        use fileFixture = new TemporaryFileCodeSample(WithErrors)

        let config = FormatConfig.Default
        let output = new StringWriter()

        let exitCode =
            fileFixture.Filename
            |> Seq.singleton
            |> Check.run config output

        exitCode |> should equal Check.ExitCode.InternalError

    [<Test>]
    let ``Files that need formatting should report that they need to be formatted``() =
        use fileFixture = new TemporaryFileCodeSample(NeedsFormatting)

        let config = FormatConfig.Default
        let output = new StringWriter()

        let exitCode =
            fileFixture.Filename
            |> Seq.singleton
            |> Check.run config output

        exitCode |> should equal Check.ExitCode.NeedsFormating
