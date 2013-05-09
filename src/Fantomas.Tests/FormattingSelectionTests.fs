module Fantomas.Tests.FormattingSelectionTests

open NUnit.Framework
open FsUnit

open Microsoft.FSharp.Compiler.Range

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

[<Test>]
let ``should format a part of a line correctly``() =
    formatSelectionFromString false (mkRange "/tmp.fs" (mkPos 3 8) (mkPos 3 12)) """
let x = 2 + 3
let y = 1+2
let z = x + y""" config
    |> prepend newline
    |> should equal """
let x = 2 + 3
let y = 1 + 2

let z = x + y
"""
