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
module internal AssemblyAttributes = 
    //[<assembly: System.Security.SecurityTransparent>]
    do()

//-------------------------------------------------------------------------------------------------
module internal Global = 
    let debugCmdLineArgs = true"""    

;;

printfn "Result:\n%s" <| formatSourceString t01 config;;

printfn "Tree:\n%A" <| parse t01;;


