module Fantomas.Tests.FormatConfigEditorConfigurationFileTests

open Fantomas
open Fantomas.EditorConfig
open Fantomas.FormatConfig
open NUnit.Framework
open System.IO
open Fantomas.Tests.TestHelper
open EditorConfig.Core
open System

[<Literal>]
let private ConfigFileName = ".editorconfig"

// TODO: Generalize/move/remove? [JB]
/// Creates a new temporary folder and allows creating files in it. Cleans the folder up on dispose.
type TemporaryFolder() =
    let folderPath = Path.Combine(Path.GetTempPath(), Guid.NewGuid().ToString())
    do Directory.CreateDirectory(folderPath) |> ignore

    member _.GetFileName(name: string) =
        Path.Combine(folderPath, name)

    member this.AddTextFile(name: string, content: string) =
        let fileName = this.GetFileName(name)
        File.WriteAllText(fileName, content)
        fileName

    interface IDisposable with
        member this.Dispose() = Directory.Delete(folderPath, true)

[<Test>]
let ``editorconfig options are parsed`` () =
    use temp = new TemporaryFolder()
    let sourceFile = temp.GetFileName("source.fs")
    temp.AddTextFile(ConfigFileName, @"root=true
        [*.fs]
        indent_style = space
        indent_size = 3
        ") |> ignore
    let opt, warns = readOptionsFromEditorConfig sourceFile
    CollectionAssert.Contains(opt, ("IndentSpaceNum", 3))
    [] == warns

[<Test>]
let ``unknown editorconfig options are silently ignored`` () =
    use temp = new TemporaryFolder()
    let sourceFile = temp.GetFileName("source.fs")
    temp.AddTextFile(ConfigFileName, @"root=true
        [*.fs]
        fsharp_mysterious_setting = abrakadabra
        ") |> ignore
    let opt, warns = readOptionsFromEditorConfig sourceFile
    [] == opt
    [] == warns
