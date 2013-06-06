#r "../../lib/FSharp.Compiler.dll"

#load "SourceFilter.fs"
#load "FormatConfig.fs"
#load "SourceParser.fs"
#load "SourceTransformer.fs"
#load "CodePrinter.fs"
#load "CodeFormatter.fs"

open Fantomas.FormatConfig
open Fantomas.SourceParser
open Fantomas.SourceFilter
open Fantomas.CodePrinter
open Fantomas.CodeFormatter

let config = FormatConfig.Default

let t01 = """
(* Comments *)
// This is another comment

/// This is doc comment

[<Test>]
let ``this is a test``() = ()
"""

let t02 = """
module ``method``

let ``abstract`` = "abstract"

/// This is doc comment
type SomeType() =
    member this.``new``() = 
        System.Console.WriteLine("Hello World!")
"""
;;

let xs = tokenize false t02 |> Array.map (fun (Token(x, y, z)) -> x, y.CharClass);;

printfn "Result:\n%s" <| formatSourceString false t02 config;;

printfn "Result:\n%s" <| formatSelectionFromString false (makeRange 6 5 6 51) t02 config;;

printfn "Tree:\n%A" <| parse false t02;;