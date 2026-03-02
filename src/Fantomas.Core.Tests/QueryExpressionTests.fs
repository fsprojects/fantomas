module Fantomas.Core.Tests.QueryExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``simple join`` () =
    formatSourceString
        """
query {
    for student in db.Student do
    join selection in db.CourseSelection
        on (student.StudentID = selection.StudentID)
    select (student, selection)
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
query {
    for student in db.Student do
    join selection in db.CourseSelection
        on (student.StudentID = selection.StudentID)
    select (student, selection)
}
"""

[<Test>]
let ``groupJoin with on and into`` () =
    formatSourceString
        """
query {
    for student in db.Student do
    groupJoin courseSelection in db.CourseSelection
        on (student.StudentID = courseSelection.StudentID) into g
    for courseSelection in g do
    join course in db.Course
        on (courseSelection.CourseID = course.CourseID)
    select (student.Name, course.CourseName)
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
query {
    for student in db.Student do
    groupJoin courseSelection in db.CourseSelection
        on (student.StudentID = courseSelection.StudentID)
        into g
    for courseSelection in g do
    join course in db.Course
        on (courseSelection.CourseID = course.CourseID)
    select (student.Name, course.CourseName)
}
"""

[<Test>]
let ``leftOuterJoin with on and into`` () =
    formatSourceString
        """
query {
    for student in db.Student do
    leftOuterJoin selection in db.CourseSelection
        on (student.StudentID = selection.StudentID) into result
    for selection in result.DefaultIfEmpty() do
    select (student, selection)
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
query {
    for student in db.Student do
    leftOuterJoin selection in db.CourseSelection
        on (student.StudentID = selection.StudentID)
        into result
    for selection in result.DefaultIfEmpty() do
    select (student, selection)
}
"""

[<Test>]
let ``multiple joins`` () =
    formatSourceString
        """
query {
    for student in db.Student do
    join courseSelection in db.CourseSelection
        on (student.StudentID = courseSelection.StudentID)
    join course in db.Course
        on (courseSelection.CourseID = course.CourseID)
    select (student.Name, course.CourseName)
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
query {
    for student in db.Student do
    join courseSelection in db.CourseSelection
        on (student.StudentID = courseSelection.StudentID)
    join course in db.Course
        on (courseSelection.CourseID = course.CourseID)
    select (student.Name, course.CourseName)
}
"""

[<Test>]
let ``multiple left outer joins`` () =
    formatSourceString
        """
query {
    for student in db.Student do
    leftOuterJoin courseSelection in db.CourseSelection
        on (student.StudentID = courseSelection.StudentID) into g1
    for courseSelection in g1.DefaultIfEmpty() do
    leftOuterJoin course in db.Course
        on (courseSelection.CourseID = course.CourseID) into g2
    for course in g2.DefaultIfEmpty() do
    select (student.Name, course.CourseName)
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
query {
    for student in db.Student do
    leftOuterJoin courseSelection in db.CourseSelection
        on (student.StudentID = courseSelection.StudentID)
        into g1
    for courseSelection in g1.DefaultIfEmpty() do
    leftOuterJoin course in db.Course
        on (courseSelection.CourseID = course.CourseID)
        into g2
    for course in g2.DefaultIfEmpty() do
    select (student.Name, course.CourseName)
}
"""

[<Test>]
let ``join with distinct`` () =
    formatSourceString
        """
query {
    for student in db.Student do
    join selection in db.CourseSelection
        on (student.StudentID = selection.StudentID)
    distinct
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
query {
    for student in db.Student do
    join selection in db.CourseSelection
        on (student.StudentID = selection.StudentID)
    distinct
}
"""

[<Test>]
let ``multiline join with on and into, 3156`` () =
    formatSourceString
        """
query {
    for persons in database do
    join items in database2
        on ((persons.LongIdName, persons.LongerIdName) = (items.LongIdName, items.LongerIdName))
        into result
    select (persons, result)
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
query {
    for persons in database do
    join items in database2
        on ((persons.LongIdName, persons.LongerIdName) = (items.LongIdName, items.LongerIdName))
        into result
    select (persons, result)
}
"""

[<Test>]
let ``groupBy with into`` () =
    formatSourceString
        """
query {
    for student in db.Student do
    groupBy student.Age into g
    select (g.Key, g.Count())
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
query {
    for student in db.Student do
    groupBy student.Age into g
    select (g.Key, g.Count())
}
"""

[<Test>]
let ``groupValBy with into`` () =
    formatSourceString
        """
query {
    for student in db.Student do
    groupValBy student.Name student.Age into g
    select (g, g.Key, g.Count())
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
query {
    for student in db.Student do
    groupValBy student.Name student.Age into g
    select (g, g.Key, g.Count())
}
"""

[<Test>]
let ``blank lines between query statements are preserved`` () =
    formatSourceString
        """
query {
    for student in db.Student do

    join selection in db.CourseSelection
        on (student.StudentID = selection.StudentID)

    select (student, selection)
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
query {
    for student in db.Student do

    join selection in db.CourseSelection
        on (student.StudentID = selection.StudentID)

    select (student, selection)
}
"""

[<Test>]
let ``nested query in async, 3156`` () =
    formatSourceString
        """
let exampleCode
    (database : dbContext)
    (id : int)
    : Async<ReturnType list> =
    async {
        return!
            query {
                for persons in database1 do
                    join items in database2
                        on ((persons.LongIdName, persons.LongerIdName) = (items.LongIdName, items.LongerIdName))
                        into result
                    for i in result.DefaultIfEmpty() do
                        select (a.LongIdName, a.AccountId)
                        distinct
            }
            |> AsyncExtensions.ToArrayAsync
            |> Async.AwaitTask
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let exampleCode (database: dbContext) (id: int) : Async<ReturnType list> =
    async {
        return!
            query {
                for persons in database1 do
                join items in database2
                    on ((persons.LongIdName, persons.LongerIdName) = (items.LongIdName, items.LongerIdName))
                    into result
                for i in result.DefaultIfEmpty() do
                select (a.LongIdName, a.AccountId)
                distinct
            }
            |> AsyncExtensions.ToArrayAsync
            |> Async.AwaitTask
    }
"""
