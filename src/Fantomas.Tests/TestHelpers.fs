module Fantomas.Tests.TestHelper

open NUnit.Framework
open FsUnit

open Fantomas.FormatConfig
open Fantomas.CodeFormatter

let config = FormatConfig.Default
let newline = "\n"
let formatSourceString fsi s config = (formatSourceString fsi s config).Replace("\r\n", "\n")
let formatSelectionFromString fsi r s config = (formatSelectionFromString fsi r s config).Replace("\r\n", "\n")

let inline prepend s content = s + content
let inline append s content = content + s
