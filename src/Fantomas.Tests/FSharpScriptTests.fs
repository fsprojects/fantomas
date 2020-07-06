module Fantomas.Tests.FSharpScriptTests

open Fantomas
open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper
open System.IO

[<Test>]
let ``source _directory keyword should not be replace with actual path`` () =
    formatSourceString false """
#I __SOURCE_DIRECTORY__
#load ".paket/load/net471/main.group.fsx"
"""  config
    |> should equal """#I __SOURCE_DIRECTORY__
#load ".paket/load/net471/main.group.fsx"
"""

[<Test>]
let ``e2e script test with keyword __source__directory__`` () =
    async {
        let source = """
#I       __SOURCE_DIRECTORY__
#load    ".paket/load/net471/main.group.fsx"
"""

        let file = Path.Combine(Path.GetTempPath(), System.Guid.NewGuid().ToString("N") + ".fsx")
        File.WriteAllText(file, source)
        
        let! formattedFiles = FakeHelpers.formatCode [file]
        
        let formattedSource = File.ReadAllText(file)
        Array.length formattedFiles == 1
        File.Delete(file)
    
        formattedSource
        |> String.normalizeNewLine
        |> should equal """#I __SOURCE_DIRECTORY__
#load ".paket/load/net471/main.group.fsx"
"""
    } |> Async.RunSynchronously

[<Test>]
let ``fantomas removes module and namespace if it is only 1 word`` () =
    async {
        let source = """namespace Shared   

    type Counter = int
    """

        let file = Path.Combine(Path.GetTempPath(), System.Guid.NewGuid().ToString("N") + ".fsx")
        File.WriteAllText(file, source)
        
        let fantomasConfig =
            { FormatConfig.FormatConfig.Default with
                StrictMode = true
                IndentSize = 2
                SpaceBeforeColon = false }
            |> EditorConfig.configToEditorConfig

        let editorConfigPath = Path.Combine(Path.GetTempPath(), ".editorconfig")
        File.WriteAllText(editorConfigPath, fantomasConfig)

        let! formattedFiles = FakeHelpers.formatCode [file]
        
        let formattedSource = File.ReadAllText(file)
        Array.length formattedFiles == 1
        File.Delete(file)
        File.Delete(editorConfigPath)
        
        formattedSource
        |> String.normalizeNewLine
        |> should equal """namespace Shared

type Counter = int
"""
    } |> Async.RunSynchronously

[<Test>]
let ``number in the filename should not end up in the module name`` () =
    let source = """let simplePatternMatch   =  
    let x = "a"
    match x with
    | "a" -> printfn "x is a"
    | "b" -> printfn "x is b"
    | _ -> printfn "x is %s"   x
"""

    let file = Path.Combine(Path.GetTempPath(), "60Seconds.fsx")
    File.WriteAllText(file, source)
    
    async {
        let! formattedFiles = FakeHelpers.formatCode [|file|]
        let formattedSource = File.ReadAllText(file)
        Array.length formattedFiles == 1
        File.Delete(file)
        
        formattedSource
        |> String.normalizeNewLine
        |> should equal """let simplePatternMatch =
    let x = "a"
    match x with
    | "a" -> printfn "x is a"
    | "b" -> printfn "x is b"
    | _ -> printfn "x is %s" x
"""
    } |> Async.RunSynchronously
