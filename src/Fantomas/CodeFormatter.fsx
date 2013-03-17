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
    type MyClass2(dataIn) as self =
       let data = dataIn
       do
           self.PrintMessage()
       member this.PrintMessage() =
           printf "Creating MyClass2 with Data %d" data"""

let t02 = """
#light "off"
let Multiple9x9 () = 
  for i in 1 .. 9 do
    printf "\n";
    for j in 1 .. 9 do
      let k = i * j in
      printf "%d x %d = %2d " i j k;
    done;
  done;;
Multiple9x9 ();;
printf "\n" ;;"""

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
let xRef : int ref = ref 10

let PrintLines3() =
    seq {
        let finished = ref false 
        while not !finished do 
            match System.Console.ReadLine() with
            | null -> finished := true
            | s -> yield s
    }"""

let t07 = """
let genericSumUnits ( x : float<'u>) (y: float<'u>) = x + y
type vector3D<[<Measure>] 'u> = { x : float<'u>; y : float<'u>; z : float<'u>}
"""

let t08 = """
let a1 = [| for i in 1 .. 10 -> i * i |]
let a2 = [| 0 .. 99 |]  
let a3 = [| for n in 1 .. 100 do if isPrime n then yield n |]
    """

let t09 = """let arr = [|(1, 1, 1); (1, 2, 2); (1, 3, 3); (2, 1, 2); (2, 2, 4); (2, 3, 6); (3, 1, 3);
  (3, 2, 6); (3, 3, 9)|]"""

let t10 = """
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
printfn "Result:\n%s" <| formatSourceString t09 config;;
printfn "Result:\n%s" <| formatSourceString t10 config;;

printfn "Tree:\n%A" <| parse t06;;