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
type BaseClass =
    val string1 : string
    new (str) = { string1 = str }
    new () = { string1 = "" }

type DerivedClass =
    inherit BaseClass
    val string2 : string
    new (str1, str2) = { inherit BaseClass(str1); string2 = str2 }
    new (str2) = { inherit BaseClass(); string2 = str2 }"""

let t02 = """
type Type
    = TyLam of Type * Type
    | TyVar of string
    | TyCon of string * Type list
    with override this.ToString() =
            match this with
            | TyLam (t1, t2) -> sprintf "(%s -> %s)" (t1.ToString()) (t2.ToString())
            | TyVar a -> a
            | TyCon (s, ts) -> s"""

let t03 = """
    type Point2D =
       struct 
          val X: float
          val Y: float
          new(x: float, y: float) = { X = x; Y = y }
       end"""

let t04 = """
    type MyClassBase1() =
       let mutable z = 0
       abstract member function1 : int -> int
       default u.function1(a : int) = z <- z + a; z

    type MyClassDerived1() =
       inherit MyClassBase1()
       override u.function1(a: int) = a + 1"""

let t05 = """
let listOfSquares = [ for i in 1 .. 10 -> i*i ]
let list0to3 = [0 .. 3]
"""

let t06 = """
let a1 = [| for i in 1 .. 10 -> i * i |]
let a2 = [| 0 .. 99 |]  
let a3 = [| for n in 1 .. 100 do if isPrime n then yield n |]
    """

let t07 = """let arr = [|(1, 1, 1); (1, 2, 2); (1, 3, 3); (2, 1, 2); (2, 2, 4); (2, 3, 6); (3, 1, 3);
  (3, 2, 6); (3, 3, 9)|]"""

let t08 = """
let array1 = [| 1; 2; 3 |]
array1.[0..2]  
array1.[1] <- 3
    """;;

printfn "Result:\n%s" <| formatSourceString t01 config;;
printfn "Result:\n%s" <| formatSourceString t02 config;;
printfn "Result:\n%s" <| formatSourceString t03 config;;
printfn "Result:\n%s" <| formatSourceString t04 config;;
printfn "Result:\n%s" <| formatSourceString t05 config;;
printfn "Result:\n%s" <| formatSourceString t06 config;;
printfn "Result:\n%s" <| formatSourceString t07 config;;
printfn "Result:\n%s" <| formatSourceString t08 config;;

printfn "Tree:\n%A" <| parse t07;;