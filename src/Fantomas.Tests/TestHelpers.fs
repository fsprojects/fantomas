module Fantomas.Tests.TestHelper

open NUnit.Framework
open FsUnit

open System
open System.IO
open Fantomas.FormatConfig
open Fantomas.CodeFormatter
open System.Reflection
open Fantomas

let config = FormatConfig.Default
let newline = "\n"

let formatSourceString isFsiFile (s : string) config = 
    // On Linux/Mac this will exercise different line endings
    let s = s.Replace("\r\n", Environment.NewLine)
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    CodeFormatter.FormatDocument(fileName, s, config).Replace("\r\n", "\n")

let formatSelectionFromString isFsiFile r (s : string) config = 
    let s = s.Replace("\r\n", Environment.NewLine)
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    CodeFormatter.FormatSelectionInDocument(fileName, r, s, config).Replace("\r\n", "\n")

let formatSelectionOnly isFsiFile r (s : string) config = 
    let s = s.Replace("\r\n", Environment.NewLine)
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    CodeFormatter.FormatSelection(fileName, r, s, config).Replace("\r\n", "\n")

let formatAroundCursor isFsiFile p (s : string) config = 
    let s = s.Replace("\r\n", Environment.NewLine)
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    CodeFormatter.FormatAroundCursor(fileName, p, s, config).Replace("\r\n", "\n")

let isValidFSharpCode isFsiFile s =
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    CodeFormatter.IsValidFSharpCode(fileName, s)

let makeRange l1 c1 l2 c2 = CodeFormatter.MakeRange(l1, c1, l2, c2)
let makePos l1 c1 = CodeFormatter.MakePos(l1, c1)

let equal x = 
    let x = 
        match box x with
        | :? String as s -> s.Replace("\r\n", "\n") |> box
        | x -> x
    equal x

let inline prepend s content = s + content
let inline append s content = content + s
