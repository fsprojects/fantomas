#r "paket:
nuget Fantomas 4.0.0-alpha-014
nuget FSharp.Compiler.Service 36.0.3
nuget Fake.Core.Target //"
#load "./.fake/script.fsx/intellisense.fsx"

open Fake.Core
open Fake.IO
open Fake.IO.Globbing.Operators
open Fantomas
open Fantomas.FormatConfig

Target.create "CheckCodeFormat" (fun _ ->
    let result =
        !!"*.fs"
        |> FakeHelpers.checkCode
        |> Async.RunSynchronously

    if result.IsValid then
        Trace.log "No files need formatting"
    elif result.NeedsFormatting then
        Trace.log "The following files need formatting:"
        List.iter Trace.log result.Formatted
        failwith "Some files need formatting, check output for more info"
    else
        Trace.logf "Errors while formatting: %A" result.Errors)

Target.create "Format" (fun _ ->
    !!"*.fs"
    |> FakeHelpers.formatCode
    |> Async.RunSynchronously
    |> printfn "Formatted files: %A")

Target.runOrList()
