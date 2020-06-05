module Fantomas.Benchmarks.Runners

open BenchmarkDotNet.Attributes
open System.IO
open FSharp.Compiler.SourceCodeServices
open Fantomas

let sharedChecker = lazy FSharpChecker.Create()
let config = Fantomas.FormatConfig.FormatConfig.Default

[<MemoryDiagnoser>]
[<RankColumn>]
[<ShortRunJob>]
type CodePrinterTest() =
    [<Benchmark>]
    member _.Format() =
        let path = __SOURCE_DIRECTORY__ + "/../../paket-files/fsprojects/fantomas/src/Fantomas/CodePrinter.fs"
        let fileName = Path.GetFileName(path)
        let parsingOptions = FakeHelpers.createParsingOptionsFromFile fileName
        let content = File.ReadAllText(path) |> SourceOrigin.SourceString
        CodeFormatter.FormatDocumentAsync(fileName, content, config, parsingOptions, sharedChecker.Value)
        |> Async.RunSynchronously
        |> ignore
        ()