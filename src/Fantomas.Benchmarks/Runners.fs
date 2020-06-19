module Fantomas.Benchmarks.Runners

open BenchmarkDotNet.Attributes
open System.IO
open FSharp.Compiler.SourceCodeServices
open Fantomas

let sharedChecker = lazy FSharpChecker.Create()
let config = Fantomas.FormatConfig.FormatConfig.Default

let private formatFile path =
    let path = __SOURCE_DIRECTORY__ + path
    let fileName = Path.GetFileName(path)
    let parsingOptions = FakeHelpers.createParsingOptionsFromFile fileName
    let content = File.ReadAllText(path) |> SourceOrigin.SourceString
    CodeFormatter.FormatDocumentAsync(fileName, content, config, parsingOptions, sharedChecker.Value)
    |> Async.RunSynchronously
    |> ignore


[<MemoryDiagnoser>]
[<RankColumn>]
[<ShortRunJob>]
type CodePrinterTest() =
    [<Benchmark>]
    member _.Format() =
        formatFile "/../../paket-files/fsprojects/fantomas/src/Fantomas/CodePrinter.fs"

[<MemoryDiagnoser>]
[<RankColumn>]
[<ShortRunJob>]
type AstTransformerTest() =
    [<Benchmark>]
    member _.Format() =
        formatFile "/../../paket-files/fsprojects/fantomas/src/Fantomas/AstTransformer.fs"