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
namespace ``method``

type SomeType() =
    member this.Print() = 
        global.System.Console.WriteLine("Hello World!")
"""
;;

printfn "Result:\n%s" <| formatSourceString false t02 config;;

printfn "Result:\n%s" <| formatSelectionFromString false (makeRange 6 5 6 51) t02 config;;

printfn "Tree:\n%A" <| parse false t02;;