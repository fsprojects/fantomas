#r "paket:
source https://api.nuget.org/v3/index.json
source ../bin
nuget FSharp.Compiler.Service 28.0.0
nuget Fantomas 3.0.0
nuget Fake.Core.Target //"
#load "./.fake/debug.fsx/intellisense.fsx"

open Fake.Core
open Fake.IO.Globbing.Operators
open Fantomas.FakeHelpers
open Fantomas.FormatConfig

let fantomasConfig =
    { FormatConfig.Default with
        StrictMode = true
        IndentSpaceNum = 2
        SpaceBeforeColon = false }

Target.create "CheckCodeFormat" (fun _ ->
    try
        !! "*.fs"
        |> checkCode fantomasConfig
    with
    | :? Fantomas.FakeHelpers.CodeFormatException as cfe ->
        printfn "%A" cfe
)

Target.create "Format" (fun _ ->
    !! "*.fs"
    |> formatCode fantomasConfig
    |> printfn "Formatted files: %A"
)

Target.runOrList()