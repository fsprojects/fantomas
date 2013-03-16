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

//let t01 = parse "let obj1 = { new System.Object() with member x.ToString() = \"F#\" }"
//
//let t02 = parse """
//    let implementer() = 
//        { new ISecond with 
//            member this.H() = ()
//            member this.J() = ()
//          interface IFirst with 
//            member this.F() = ()
//            member this.G() = () }"""
//
//let t03 = parse """
//    let iterate1 (f : unit -> seq<int>) =
//        for e in f() do printfn "%d" e
//    let iterate2 (f : unit -> #seq<int>) =
//        for e in f() do printfn "%d" e"""
//
//let t04 = parse """
//    let base1 = d1 :> Base1
//    let derived1 = base1 :?> Derived1"""

let t05 = parse """
    type Delegate1 = delegate of (int * int) -> int
    type Delegate2 = delegate of int * int -> int"""

let t06 = parse """
    let private myPrivateObj = new MyPrivateType()
    let internal myInternalObj = new MyInternalType()"""

let t07 = parse """
    assert (3 > 2)
    let result = lazy (x + 10)"""

let t08 = parse """
open System
let lookForValue value maxValue =
  let mutable continueLooping = true 
  let randomNumberGenerator = new Random()
  while continueLooping do 
    // Generate a random number between 1 and maxValue. 
    let rand = randomNumberGenerator.Next(maxValue)
    printf "%d " rand
    if rand = value then 
       printfn "\nFound a %d!" value
       continueLooping <- false
lookForValue 10 20"""

let t09 = parse """
    let function2() =
      for i in 1 .. 2 .. 10 do
         printf "%d " i
      printfn ""
    function2()"""

let t10 = parse """
    let writetofile filename obj =
     use file1 = File.CreateText(filename)
     file1.WriteLine("{0}", obj.ToString())
    """

let t11 = parse """
    namespace Core
    type A = A
    """;;

//printfn "Result:\n%s" <| format t01 config;;
//printfn "Result:\n%s" <| format t02 config;;
//printfn "Result:\n%s" <| format t03 config;;
//printfn "Result:\n%s" <| format t04 config;;
printfn "Result:\n%s" <| format t05 config;;
printfn "Result:\n%s" <| format t06 config;;
printfn "Result:\n%s" <| format t07 config;;
printfn "Result:\n%s" <| format t08 config;;
printfn "Result:\n%s" <| format t09 config;;
printfn "Result:\n%s" <| format t10 config;;
printfn "Result:\n%s" <| format t11 config;;