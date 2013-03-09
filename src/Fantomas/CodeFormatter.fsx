#r "../../lib/FSharp.Compiler.dll"

#load "PrettyPrinter.fs"
#load "CodeFormatter.fs"

open Fantomas.PrettyPrinter
open Fantomas.CodeFormatter

let config = FormatConfig.Default

let t01 = parse "module MyModule = let x = 42"

let t02 = parse """
    open System
    open System.IO"""

let t03 = parse """
    type AParameters = { a : int }
    type X = | A of AParameters | B
    let f (r : X) =
        match r with
        | X.A ({ a = aValue } as t) -> aValue
        | X.B -> 0"""

let t04 = parse """
    [<Dependency("FSharp.Compiler", LoadHint.Always)>]
    do ()"""

let t05 = parse "module ES = Microsoft.FSharp.Quotations.ExprShape"

let t06 = parse """
    [<AttributeUsage(AttributeTargets.Method, AllowMultiple = true)>]
    type TestAttribute([<ParamArray>] parameters: obj[])  =
        inherit Attribute()
        member this.Parameters = parameters"""

#time "on";;

printfn "Result:\n%s" <| format t01 config;;
printfn "Result:\n%s" <| format t02 config;;
printfn "Result:\n%s" <| format t03 config;;
printfn "Result:\n%s" <| format t04 config;;
printfn "Result:\n%s" <| format t05 config;;
printfn "Result:\n%s" <| format t06 config;;