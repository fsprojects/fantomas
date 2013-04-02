#r "../../lib/FSharp.Compiler.dll"

#load "FormatConfig.fs"
#load "SourceParser.fs"
#load "CodePrinter.fs"
#load "CodeFormatter.fs"

open Fantomas.FormatConfig
open Fantomas.SourceParser
open Fantomas.CodePrinter
open Fantomas.CodeFormatter

let config = FormatConfig.Default

let t01 = """
let lexBuffer = 
    new LexBuffer<_> 
        { fillSync = Some (fun _ -> ()); 
            fillAsync = Some (fun _ -> async { return () }) }"""    

;;

printfn "Result:\n%s" <| formatSourceString t01 config;;

printfn "Tree:\n%A" <| parse t01;;