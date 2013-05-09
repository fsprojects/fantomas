#r "../../lib/FSharp.Compiler.dll"

#load "FormatConfig.fs"
#load "SourceParser.fs"
#load "SourceTransformer.fs"
#load "CodePrinter.fs"
#load "CodeFormatter.fs"

open Microsoft.FSharp.Compiler.Range

open Fantomas.FormatConfig
open Fantomas.SourceParser
open Fantomas.CodePrinter
open Fantomas.CodeFormatter

let config = FormatConfig.Default

let t01 = """
module TopLevel

let topLevelX = 5

module Inner1 =
    let inner1X = 1
module Inner2 =
    let inner2X = 5
"""

let t02 = """
let x = 2 + 3
let y = 1+2
let z = x + y"""
;;

printfn "Result:\n%s" <| formatSourceString false t01 config;;

printfn "Result:\n%s" 
<| formatSelectionFromString false (mkRange "/tmp.fs" (mkPos 3 8) (mkPos 3 12)) t02 config;;

printfn "Tree:\n%A" <| parse false t02;;