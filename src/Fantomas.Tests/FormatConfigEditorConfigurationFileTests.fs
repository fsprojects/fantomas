module Fantomas.Tests.FormatConfigEditorConfigurationFileTests

open System
open Fantomas
open Fantomas.FormatConfig
open NUnit.Framework
open System.IO
open Fantomas.Tests.TestHelper

let private defaultConfig = FormatConfig.Default
let private tempName () = System.Guid.NewGuid().ToString("N")

type ConfigurationFile internal (config: Fantomas.FormatConfig.FormatConfig,
                                 ?editorConfigHeader: string,
                                 ?subFolder: string,
                                 ?isRoot: bool,
                                 ?content: string) =
    let editorConfigPath =
        match subFolder with
        | Some sf ->
            let dirPath = Path.Join(Path.GetTempPath(), sf)
            if not (Directory.Exists(dirPath))
            then Directory.CreateDirectory(dirPath) |> ignore
            Path.Join(Path.GetTempPath(), sf, ".editorconfig")
        | None -> Path.Join(Path.GetTempPath(), ".editorconfig")

    let header =
        Option.defaultValue "[*.fs]" editorConfigHeader

    let content =
        match content with
        | Some c -> c
        | None ->
            let root =
                match isRoot with
                | Some true -> "root=true"
                | _ -> String.empty

            sprintf "%s\n\n%s\n%s" root header (EditorConfig.configToEditorConfig config)

    do File.WriteAllText(editorConfigPath, content)

    interface IDisposable with
        member this.Dispose(): unit = if File.Exists(editorConfigPath) then File.Delete(editorConfigPath)

type FSharpFile internal (?fsharpFileExtension: string, ?subFolder: string) =
    let extension =
        Option.defaultValue ".fs" fsharpFileExtension

    let fsharpFile = sprintf "%s%s" (tempName ()) extension

    let fsharpFilePath =
        match subFolder with
        | Some sf ->
            let dirPath = Path.Join(Path.GetTempPath(), sf)
            if not (Directory.Exists(dirPath))
            then Directory.CreateDirectory(dirPath) |> ignore
            Path.Join(Path.GetTempPath(), sf, fsharpFile)
        | None -> Path.Join(Path.GetTempPath(), fsharpFile)

    do File.WriteAllText(fsharpFilePath, String.empty)

    member _.FSharpFile: string = fsharpFilePath

    interface IDisposable with
        member this.Dispose(): unit = if File.Exists(fsharpFilePath) then File.Delete(fsharpFilePath)

[<Test>]
let ``single configuration file`` () =
    use configFixture = new ConfigurationFile(defaultConfig)
    use fsharpFile = new FSharpFile(".fs")

    let config =
        CodeFormatter.ReadConfiguration fsharpFile.FSharpFile

    config == defaultConfig

[<Test>]
let ``pointing to subfolder should return parent config file as well`` () =
    let subFolder = tempName ()

    use parentConfig =
        new ConfigurationFile({ defaultConfig with IndentSize = 3 })

    use childConfig =
        new ConfigurationFile({ defaultConfig with IndentSize = 2 }, subFolder = subFolder)

    use fsharpFile = new FSharpFile(subFolder = subFolder)

    let config =
        CodeFormatter.ReadConfiguration fsharpFile.FSharpFile

    config.IndentSize == 2

[<Test>]
let ``parent config should not be taking into account when child is root`` () =
    let subFolder = tempName ()

    use parentConfig =
        new ConfigurationFile({ defaultConfig with
                                    MaxRecordWidth = 10 })

    use childConfig =
        new ConfigurationFile({ defaultConfig with IndentSize = 2 }, subFolder = subFolder, isRoot = true)

    use fsharpFile = new FSharpFile(subFolder = subFolder)

    let config =
        CodeFormatter.ReadConfiguration fsharpFile.FSharpFile

    config.MaxRecordWidth
    == defaultConfig.MaxRecordWidth
    config.IndentSize == 2

[<Test>]
let ``configuration file should not affect file extension`` () =
    use configFixture =
        new ConfigurationFile({ defaultConfig with
                                    MaxLineLength = 90 })

    use fsharpFile = new FSharpFile(".fsx")

    let config =
        CodeFormatter.ReadConfiguration fsharpFile.FSharpFile

    config.MaxLineLength
    == defaultConfig.MaxLineLength

[<Test>]
let ``fantomas-tool configuration file`` () =
    let myConfig = """
[*.fs]
fsharp_max_if_then_else_short_width=25
fsharp_max_value_binding_width=40
fsharp_max_function_binding_width=40
"""

    use configFixture =
        new ConfigurationFile(defaultConfig, content = myConfig)

    use fsharpFile = new FSharpFile()

    let config =
        CodeFormatter.ReadConfiguration fsharpFile.FSharpFile

    config.MaxIfThenElseShortWidth == 25
    config.MaxValueBindingWidth == 40
    config.MaxFunctionBindingWidth == 40

[<Test>]
let ``non existing file should return defaults for readConfiguration`` () =
    use configFixture = new ConfigurationFile(defaultConfig)

    let config =
        CodeFormatter.ReadConfiguration "bogus.fs"

    config == defaultConfig

    // In the future we could ensure that the Default config isn't 
    // being generated every time because it's a property getter     
    // Assert.That(Object.ReferenceEquals(config, defaultConfig))

[<Test>]
let ``non existing file should return None for tryReadConfiguration`` () =
    use configFixture = new ConfigurationFile(defaultConfig)

    let config =
        CodeFormatter.TryReadConfiguration "bogus.fs"

    config == None

[<Test>]
let ``indent_style tab edge case`` () =
    let editorConfig = """
[*.fs]
indent_style=tab
indent_size=tab
tab_width=5
fsharp_indent_on_try_with=true
"""

    use configFixture =
        new ConfigurationFile(defaultConfig, content = editorConfig)

    use fsharpFile = new FSharpFile()

    let config =
        CodeFormatter.ReadConfiguration fsharpFile.FSharpFile

    config.IndentSize == 5
