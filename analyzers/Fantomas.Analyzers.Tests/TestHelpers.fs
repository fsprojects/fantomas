module Fantomas.Analyzers.Tests.TestHelpers

open System.Threading.Tasks
open NUnit.Framework
open FSharp.Compiler.CodeAnalysis
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.Testing

/// Building the project options means creating a class library in a temporary folder and building
/// it, which takes long enough that doing it per test would dominate the run. Every test in this
/// assembly analyzes a snippet against the same empty project, so one set is built for all of them.
///
/// The framework has to be one the machine can build. net10.0 is what `global.json` pins, so it is
/// present wherever this repository builds at all. It was net8.0 first, which passed on a developer
/// machine with an old SDK still installed and failed in the dev container, where only .NET 10
/// lives.
let mutable private projectOptions: FSharpProjectOptions = FSharpProjectOptions.zero

[<SetUpFixture>]
type ProjectOptionsFixture() =
    [<OneTimeSetUp>]
    member _.Setup() : Task =
        task {
            let! fresh = mkOptionsFromProject "net10.0" []

            // The helper builds a bare class library, so its command line is a fresh project's
            // defaults. Of everything this repository adds in Directory.Build.props, only
            // `--strict-indentation+` reaches the parser, so a snippet is held to the same standard
            // as the code the rules run over. `--realsig+` and the `--test:` switches are for later
            // compiler phases and cannot change a tree, and LangVersion is never set here at all, as
            // a design time build of Fantomas.Core confirms.
            let options: FSharpProjectOptions =
                { fresh with
                    OtherOptions = Array.append fresh.OtherOptions [| "--strict-indentation+" |] }

            // `mkOptionsFromProject` catches whatever goes wrong and hands back options with
            // nothing in them, which shows up much later as an exception about critical errors in
            // the project options. Say so here instead, while the cause is still in reach.
            if Array.isEmpty options.OtherOptions then
                failwith
                    "Could not build the project options for the tests. `mkOptionsFromProject` runs `dotnet new classlib` and `dotnet build` in a temporary folder, so this usually means the SDK or a package feed was not available."

            projectOptions <- options
        }

/// Runs an analyzer over a snippet of implementation source.
let analyzeSource (analyzer: Analyzer<CliContext>) (source: string) : Message list =
    let ctx: CliContext = getContext projectOptions source
    analyzer ctx |> Async.RunSynchronously

/// Runs an analyzer over an implementation file that has a signature file beside it, which is what
/// the rules keyed on the signature file need in order to see one.
let analyzeWithSignature (analyzer: Analyzer<CliContext>) (signature: string) (implementation: string) : Message list =
    let signatureFile: SourceFile =
        { FileName = "M.fsi"
          Source = signature }

    let implementationFile: SourceFile =
        { FileName = "M.fs"
          Source = implementation }

    let ctx: CliContext =
        getContextFor
            (AnalyzerProjectOptions.BackgroundCompilerOptions projectOptions)
            [ signatureFile; implementationFile ]
            implementationFile
        |> Async.AwaitTask
        |> Async.RunSynchronously

    analyzer ctx |> Async.RunSynchronously

/// Asserts which one-based lines a set of messages points at. Comparing lines rather than whole
/// messages keeps a test readable and survives a reworded message.
let assertLines (expected: int list) (messages: Message list) : unit =
    let actual: int list =
        messages |> List.map (fun (message: Message) -> message.Range.StartLine)

    Assert.That(actual, Is.EqualTo<int list> expected)
