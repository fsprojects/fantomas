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

let t01 = parse """
    type MyClass2(dataIn) as self =
       let data = dataIn
       do
           self.PrintMessage()
       member this.PrintMessage() =
           printf "Creating MyClass2 with Data %d" data"""

let t02 = parse """
    type MyClass1(x: int, y: int) =
     do printfn "%d %d" x y
     new() = MyClass1(0, 0)"""

let t03 = parse """
    type Point2D =
       struct 
          val X: float
          val Y: float
          new(x: float, y: float) = { X = x; Y = y }
       end"""

let t04 = parse """
    type MyClassBase1() =
       let mutable z = 0
       abstract member function1 : int -> int
       default u.function1(a : int) = z <- z + a; z

    type MyClassDerived1() =
       inherit MyClassBase1()
       override u.function1(a: int) = a + 1"""

let t05 = parse """
    type Delegate1 = delegate of (int * int) -> int
    type Delegate2 = delegate of int * int -> int"""

//let t06 = parse """
//    let private myPrivateObj = new MyPrivateType()
//    let internal myInternalObj = new MyInternalType()"""

//let t07 = parse """
//    assert (3 > 2)
//    let result = lazy (x + 10)
//    do printfn "Hello world"
//"""

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

//let t09 = parse """
//    let function2() =
//      for i in 1 .. 2 .. 10 do
//         printf "%d " i
//      printfn ""
//    function2()"""

//let t10 = parse """
//    let writetofile filename obj =
//     use file1 = File.CreateText(filename)
//     file1.WriteLine("{0}", obj.ToString())
//    """

let t11 = parse """
    namespace Core
    type A = A
    """;;

printfn "Result:\n%s" <| format t01 config;;
printfn "Result:\n%s" <| format t02 config;;
printfn "Result:\n%s" <| format t03 config;;
printfn "Result:\n%s" <| format t04 config;;
printfn "Result:\n%s" <| format t05 config;;
//printfn "Result:\n%s" <| format t06 config;;
//printfn "Result:\n%s" <| format t07 config;;
printfn "Result:\n%s" <| format t08 config;;
//printfn "Result:\n%s" <| format t09 config;;
//printfn "Result:\n%s" <| format t10 config;;
printfn "Result:\n%s" <| format t11 config;;