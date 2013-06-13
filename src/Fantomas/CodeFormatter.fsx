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
type BaseClass = class
    val string1 : string
    new (str) = { string1 = str }
    new () = { string1 = "" }
end
"""

let t02 = """
try 
    fst(find (fun (s, (s', ty)) -> 
                s' = s0 && can (type_match ty ty0) []) (!the_interface))
with
| Failure _ -> s0
"""
;;

printfn "Result:\n%s" <| formatSourceString false t02 config;;

printfn "Result:\n%s" <| formatSelectionFromString false (makeRange 6 5 6 51) t02 config;;

printfn "Tree:\n%A" <| parse false t02;;