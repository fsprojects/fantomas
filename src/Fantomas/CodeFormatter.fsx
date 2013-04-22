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
let pattern = 
    (x + y)
      .Replace(seperator + "**" + seperator, replacementSeparator + "(.|?" + replacementSeparator + ")?" )
      .Replace("**" + seperator, ".|(?<=^|" + replacementSeparator + ")" )
"""

let t02 = """
type IntegerRegex = FSharpx.Regex< @"(?<value>\d+)" >
"""
;;

printfn "Result:\n%s" <| formatSourceString false t02 config;;

printfn "Tree:\n%A" <| parse false t02;;