module Fantomas.Analyzers.Tests.TestHelpers

open System.Threading.Tasks
open NUnit.Framework
open FSharp.Compiler.CodeAnalysis
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.Testing

/// Building the project options means creating a class library in a temporary folder and restoring
/// it, which takes long enough that doing it per test would dominate the run. Every test in this
/// assembly analyzes a snippet against the same empty project, so one set is built for all of them.
let mutable private projectOptions: FSharpProjectOptions = FSharpProjectOptions.zero

[<SetUpFixture>]
type ProjectOptionsFixture() =
    [<OneTimeSetUp>]
    member _.Setup() : Task =
        task {
            let! options = mkOptionsFromProject "net8.0" []
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
