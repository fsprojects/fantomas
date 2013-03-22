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
type Person(nameIn : string, idIn : int) =
    let mutable name = nameIn
    let mutable id = idIn
    do printfn "Created a person object." 
    member this.Name with get() = name and set(v) = name <- v
    member this.ID with get() = id and set(v) = id <- v
    new() = 
        Person("Invalid Name", -1)
        then
            printfn "Created an invalid person object."
            """

let t02 = """
type NumberStrings() =
   let mutable ordinals = [| "one"; "two"; "three"; "four"; "five";
                             "six"; "seven"; "eight"; "nine"; "ten" |]
   let mutable cardinals = [| "first"; "second"; "third"; "fourth";
                              "fifth"; "sixth"; "seventh"; "eighth";
                              "ninth"; "tenth" |]
   member this.Item
      with get(index) = ordinals.[index]
      and set index value = ordinals.[index] <- value
   member this.Ordinal
      with get(index) = ordinals.[index]
      and set index value = ordinals.[index] <- value
   member this.Cardinal
      with get(index) = cardinals.[index]
      and set index value = cardinals.[index] <- value"""

let t03 = """
open System.Collections.Generic
type SparseMatrix() =
    let mutable table = new Dictionary<int * int, float>()
    member this.Item
        with get(key1, key2) = table.[(key1, key2)]
        and set (key1, key2) value = table.[(key1, key2)] <- value

let matrix1 = new SparseMatrix()
for i in 1..1000 do
    matrix1.[i, i] <- float i * float i
    """

let t04 = """let arr = [|(1, 1, 1); (1, 2, 2); (1, 3, 3); (2, 1, 2); (2, 2, 4); (2, 3, 6); (3, 1, 3);
  (3, 2, 6); (3, 3, 9)|]"""

let t05 = """
let array1 = [| 1; 2; 3 |]
array1.[0..2]  
array1.[1] <- 3
    """

let t06 = """
query {
    for student in db.Student do
    groupJoin courseSelection in db.CourseSelection on
               (student.StudentID = courseSelection.StudentID) into g
    for courseSelection in g do
    join course in db.Course on (courseSelection.CourseID = course.CourseID)
    select (student.Name, course.CourseName)
    }"""    

let t07 = """
    let function1 x y =
       try 
         try 
            if x = y then raise (InnerError("inner"))
            else raise (OuterError("outer"))
         with
          | InnerError(str) -> printfn "Error1 %s" str
       finally
          printfn "Always print this."
    """

let t08 = """
let x = 1::[2; 3; 4]
let y = 1::[2]
let z = 1::[]
type Delegate1 = delegate of (int * int) -> int
type Delegate2 = delegate of int * int -> int
"""
;;

printfn "Result:\n%s" <| formatSourceString t01 config;;
printfn "Result:\n%s" <| formatSourceString t02 config;;
printfn "Result:\n%s" <| formatSourceString t03 config;;
printfn "Result:\n%s" <| formatSourceString t04 config;;
printfn "Result:\n%s" <| formatSourceString t05 config;;
printfn "Result:\n%s" <| formatSourceString t06 config;;
printfn "Result:\n%s" <| formatSourceString t07 config;;
printfn "Result:\n%s" <| formatSourceString t08 config;;

printfn "Tree:\n%A" <| parse t08;;
