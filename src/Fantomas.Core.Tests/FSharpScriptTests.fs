module Fantomas.Core.Tests.FSharpScriptTests

open System.IO
open NUnit.Framework
open FsUnit
open Fantomas.Core
open Fantomas.Extras
open Fantomas.Core.Tests.TestHelper
open Fantomas.Core.Tests.FormatConfigEditorConfigurationFileTests

[<Test>]
let ``source _directory keyword should not be replace with actual path`` () =
    formatSourceString
        false
        """
#I __SOURCE_DIRECTORY__
#load ".paket/load/net471/main.group.fsx"
"""
        config
    |> should
        equal
        """#I __SOURCE_DIRECTORY__
#load ".paket/load/net471/main.group.fsx"
"""

[<Test>]
let ``e2e script test with keyword __source__directory__`` () =
    async {
        let source =
            """
#I       __SOURCE_DIRECTORY__
#load    ".paket/load/net471/main.group.fsx"
"""

        let rootFolderName = tempName ()

        use fsharpScript =
            new FSharpFile(rootFolderName, fsharpFileExtension = ".fsx", content = source)

        let! formattedFiles = FakeHelpers.formatCode [ fsharpScript.FSharpFile ]

        let formattedSource = File.ReadAllText(fsharpScript.FSharpFile)

        Array.length formattedFiles == 1

        formattedSource
        |> String.normalizeNewLine
        |> should
            equal
            """#I __SOURCE_DIRECTORY__
#load ".paket/load/net471/main.group.fsx"
"""
    }
    |> Async.RunSynchronously

[<Test>]
let ``fantomas removes module and namespace if it is only 1 word`` () =
    async {
        let source =
            """namespace Shared

    type Counter = int
    """

        let rootFolderName = tempName ()

        use fsharpScript =
            new FSharpFile(rootFolderName, fsharpFileExtension = ".fsx", content = source)

        let fantomasConfig =
            { FormatConfig.FormatConfig.Default with
                StrictMode = true
                IndentSize = 2
                SpaceBeforeColon = false }

        use _editorConfig = new ConfigurationFile(fantomasConfig, rootFolderName)

        let! formattedFiles = FakeHelpers.formatCode [ fsharpScript.FSharpFile ]

        let formattedSource = File.ReadAllText fsharpScript.FSharpFile
        Array.length formattedFiles == 1

        formattedSource
        |> String.normalizeNewLine
        |> should
            equal
            """namespace Shared

type Counter = int
"""
    }
    |> Async.RunSynchronously

[<Test>]
let ``number in the filename should not end up in the module name`` () =
    let source =
        """let simplePatternMatch   =
    let x = "a"
    match x with
    | "a" -> printfn "x is a"
    | "b" -> printfn "x is b"
    | _ -> printfn "x is %s"   x
"""

    let rootFolderName = tempName ()

    use fsharpScript =
        new FSharpFile(rootFolderName, fileName = "60Seconds.fsx", content = source)

    async {
        let! formattedFiles = FakeHelpers.formatCode [| fsharpScript.FSharpFile |]
        let formattedSource = File.ReadAllText fsharpScript.FSharpFile
        Array.length formattedFiles == 1

        formattedSource
        |> String.normalizeNewLine
        |> should
            equal
            """let simplePatternMatch =
    let x = "a"

    match x with
    | "a" -> printfn "x is a"
    | "b" -> printfn "x is b"
    | _ -> printfn "x is %s" x
"""
    }
    |> Async.RunSynchronously
