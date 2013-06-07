#r "../../lib/FSharp.Compiler.dll"

#load "SourceFilter.fs"
#load "FormatConfig.fs"
#load "SourceParser.fs"
#load "SourceTransformer.fs"
#load "CodePrinter.fs"
#load "CodeFormatter.fs"

open Fantomas.SourceFilter
open Fantomas.FormatConfig
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
type MyClass2(dataIn) as self =
       let data = dataIn
       do self.PrintMessage()
       // Print a message to console
       member this.PrintMessage() =
           printf "Creating MyClass2 with Data %d" data"""
;;

let xs = filterComments (tokenize false t02) 
         |> Seq.iter (fun (KeyValue(pos, s)) -> printfn "l:%O, c:%O, %s" pos.Line pos.Column s);;

printfn "Result:\n%s" <| formatSourceString false t02 config;;

printfn "Result:\n%s" <| formatSelectionFromString false (makeRange 6 5 6 51) t02 config;;

printfn "Tree:\n%A" <| parse false t02;;