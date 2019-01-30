#r "paket:
nuget FSharp.Core 4.5.0.0
nuget Fantomas
nuget Fake.Core.Target //"
#load "./.fake/script.fsx/intellisense.fsx"

open Fake.Core
open Fake.IO.Globbing.Operators
open Fantomas.FakeHelpers
open Fantomas.FormatConfig

let fantomasConfig =
    { FormatConfig.Default with
            ReorderOpenDeclaration = true }

Target.create "CheckCodeFormat" (fun _ ->
    !! "*.fs"
    |> checkCode fantomasConfig
)

Target.create "Format" (fun _ ->
    !! "*.fs"
    |> formatCode fantomasConfig
    |> printfn "Formatted files: %A"
)

Target.runOrList()