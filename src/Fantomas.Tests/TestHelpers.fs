module Fantomas.Tests.TestHelper

open NUnit.Framework
open FsUnit

open System
open System.IO
open Fantomas.FormatConfig
open Fantomas.CodeFormatter
open System.Reflection

// FSharp.Compiler.Service expects FSharp.Core 4.0.0.0 in a standard location
// Since we only have FSharp.Core along with the project, we inject the custom path to project checker options.
do
    let localPath = Uri(Assembly.GetExecutingAssembly().CodeBase).LocalPath 
    let fsharpCorePath = Path.Combine(Path.GetDirectoryName(localPath), "FSharp.Core.dll")
    OverridenFSharpCorePath <- Some fsharpCorePath
    printfn "Loading FSharp.Core from %s" fsharpCorePath

let config = FormatConfig.Default
let newline = "\n"

let formatSourceString isFsiFile (s : string) config = 
    // On Linux/Mac this will exercise different line endings
    let s = s.Replace("\r\n", Environment.NewLine)
    (formatSourceString isFsiFile s config).Replace("\r\n", "\n")

let formatSelectionFromString isFsiFile r (s : string) config = 
    let s = s.Replace("\r\n", Environment.NewLine)
    (formatSelectionFromString isFsiFile r s config).Replace("\r\n", "\n")

let formatSelectionOnly isFsiFile r (s : string) config = 
    let s = s.Replace("\r\n", Environment.NewLine)
    (formatSelectionOnly isFsiFile r s config).Replace("\r\n", "\n")

let formatAroundCursor isFsiFile p (s : string) config = 
    let s = s.Replace("\r\n", Environment.NewLine)
    (formatAroundCursor isFsiFile p s config).Replace("\r\n", "\n")

let equal x = 
    let x = 
        match box x with
        | :? String as s -> s.Replace("\r\n", "\n") |> box
        | x -> x
    equal x

let inline prepend s content = s + content
let inline append s content = content + s
