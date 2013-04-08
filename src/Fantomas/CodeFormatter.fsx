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
module WSNModeller.Utils

val turnTracingOn : unit -> unit
val turnTracingOff : unit -> unit
val isTraced : unit -> bool

module Random = begin
    val exponential : mean:float -> float
    val nextInt : max:int -> int
    val nextInt64 : max:int64 -> int64
    val next : max:float -> float
end"""
;;

printfn "Result:\n%s" <| formatSourceString true t01 config;;

printfn "Tree:\n%A" <| parse true t01;;