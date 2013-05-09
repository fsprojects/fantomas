#r "../../lib/FSharp.Compiler.dll"

#load "FormatConfig.fs"
#load "SourceParser.fs"
#load "SourceTransformer.fs"
#load "CodePrinter.fs"
#load "CodeFormatter.fs"

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
let rangeTest testValue mid size =
    match testValue with
    | var1 when var1 >= mid - size/2 && var1 <= mid + size/2 -> printfn "The test value is in range."
    | _ -> printfn "The test value is out of range."

let (var1, var2) as tuple1 = (1, 2)"""
;;

printfn "Result:\n%s" <| formatSourceString false t01 config;;

printfn "Result:\n%s" <| formatSelectionFromString false (makeRange 3 5 5 52) t02 config;;

printfn "Tree:\n%A" <| parse false t02;;