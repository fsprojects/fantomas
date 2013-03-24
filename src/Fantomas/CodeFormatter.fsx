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
query {
    for student in db.Student do
    groupJoin courseSelection in db.CourseSelection on
               (student.StudentID = courseSelection.StudentID) into g
    for courseSelection in g do
    join course in db.Course on (courseSelection.CourseID = course.CourseID)
    select (student.Name, course.CourseName)
    }"""    

;;

printfn "Result:\n%s" <| formatSourceString t01 config;;

printfn "Tree:\n%A" <| parse t01;;


