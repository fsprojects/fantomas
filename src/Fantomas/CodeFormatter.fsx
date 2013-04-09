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
exception Error2 of string * int

type MyType =
  class
    new : unit -> MyType
    val mutable myInt2: int
    val mutable myString: string
    member SetValsAndPrint : i:int * str:string -> unit
  end
"""
;;

printfn "Result:\n%s" <| formatSourceString true t01 config;;

printfn "Tree:\n%A" <| parse true t01;;