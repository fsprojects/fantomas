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
open System.Collections.Generic
type SparseMatrix() =
    let mutable table = new Dictionary<int * int, float>()
    member this.Item
        with get(key1, key2) = table.[(key1, key2)]
        and set (key1, key2) value = table.[(key1, key2)] <- value

let matrix1 = new SparseMatrix()
for i in 1..1000 do
    matrix1.[i, i] <- float i * float i
"""

let t02 = """
let x = 2 + 3
let y = 1+2
let z = x + y
"""
;;

printfn "Result:\n%s" 
<| formatSelectionFromString false (mkRange "/tmp.fs" (mkPos 3 8) (mkPos 3 12)) t02 config;;

printfn "Tree:\n%A" <| parse false t02;;