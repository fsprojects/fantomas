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
query {
    for student in db.Student do
    groupJoin courseSelection in db.CourseSelection on
               (student.StudentID = courseSelection.StudentID) into g
    for courseSelection in g do
    join course in db.Course on (courseSelection.CourseID = course.CourseID)
    select (student.Name, course.CourseName)
    }"""    

let t03 = """
type Delegate1 = delegate of (int * int) -> int
type Delegate2 = delegate of int * int -> int
"""
;;

printfn "Result:\n%s" <| formatSourceString t01 config;;
printfn "Result:\n%s" <| formatSourceString t02 config;;
printfn "Result:\n%s" <| formatSourceString t03 config;;

printfn "Tree:\n%A" <| parse t03;;


