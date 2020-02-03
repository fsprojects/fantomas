#r "paket:
nuget Fantomas 3.1.0
nuget Fake.Core.Target //"
#load "./.fake/script.fsx/intellisense.fsx"

open Fake.Core
open Fake.IO.Globbing.Operators
open Fantomas.FakeHelpers
open Fantomas.FormatConfig

let fantomasConfig = { FormatConfig.Default with ReorderOpenDeclaration = true }

Target.create "CheckCodeFormat" (fun _ ->
    !!"*.fs"
    |> checkCode fantomasConfig
    |> Async.RunSynchronously)

Target.create "Format" (fun _ ->
    !!"*.fs"
    |> formatCode fantomasConfig
    |> Async.RunSynchronously
    |> printfn "Formatted files: %A")

Target.runOrList()
