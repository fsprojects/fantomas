#r "../../lib/FSharp.Compiler.dll"

#load "SourceParser.fs"
#load "FormatConfig.fs"
#load "CodePrinter.fs"
#load "CodeFormatter.fs"

open Fantomas.SourceParser
open Fantomas.FormatConfig
open Fantomas.CodePrinter
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

let t07 = parse """
    let rec f x = g x
    and g x = x"""

let t08 = parse """
    module Tests
    /// This is a foo function
    let foo() = 
        // Line comment
        let msg = "Hello world"
        printfn "%s" msg"""

let t09 = parse """
    #r "Fantomas.Tests.dll"
    #load "Tests.fs"
    """

let t10 = parse """
    exception Error2 of string * int
    with member __.Message = "ErrorMessage"
    """

printfn "Result:\n%s" <| format t01 config;;
printfn "Result:\n%s" <| format t02 config;;
printfn "Result:\n%s" <| format t03 config;;
printfn "Result:\n%s" <| format t04 config;;
printfn "Result:\n%s" <| format t05 config;;
printfn "Result:\n%s" <| format t06 config;;
printfn "Result:\n%s" <| format t07 config;;
printfn "Result:\n%s" <| format t08 config;;
printfn "Result:\n%s" <| format t09 config;;
printfn "Result:\n%s" <| format t10 config;;