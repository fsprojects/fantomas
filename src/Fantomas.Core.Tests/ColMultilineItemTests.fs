module Fantomas.Core.Tests.ColMultilineItemTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

[<Test>]
let ``two short let binding should not have extra newline`` () =
    formatSourceString
        false
        """
let a = 2
let b =  3
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = 2
let b = 3
"""

[<Test>]
let ``three short let binding should not have extra newline`` () =
    formatSourceString
        false
        """
let a = 2
let b =  3
let c =   4
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = 2
let b = 3
let c = 4
"""

[<Test>]
let ``two short expressions inside let binding`` () =
    formatSourceString
        false
        """
let b () =
    printfn "meh"
    80.7
"""
        config
    |> prepend newline
    |> should
        equal
        """
let b () =
    printfn "meh"
    80.7
"""

[<Test>]
let ``short let binding followed by long let binding`` () =
    formatSourceString
        false
        """
let a =   9
let b () =
    printfn "meh"
    80.7
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = 9

let b () =
    printfn "meh"
    80.7
"""

[<Test>]
let ``long let binding followed by short let binding`` () =
    formatSourceString
        false
        """
let b () =
    printfn "meh"
    80.7
let a =   9
"""
        config
    |> prepend newline
    |> should
        equal
        """
let b () =
    printfn "meh"
    80.7

let a = 9
"""

[<Test>]
let ``three long let bindings`` () =
    formatSourceString
        false
        """
let a =
    // some comment
    42
let b (x:int) (y:int):int =
    printfn "doing b with %i %i" x y
    x + y
let c () =
    try
        0
    with ex -> 1
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    // some comment
    42

let b (x: int) (y: int) : int =
    printfn "doing b with %i %i" x y
    x + y

let c () =
    try
        0
    with ex ->
        1
"""

[<Test>]
let ``two short let bindings with existing newlines between`` () =
    formatSourceString
        false
        """
let a = 9


let x =  70
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = 9


let x = 70
"""

[<Test>]
let ``one let binding, newline, long let binding, newline, short let binding`` () =
    formatSourceString
        false
        """
let a =  0

let b (x: int) (y: int) : int =
    printfn "doing b with %i %i" x y
    x +   y

let c =  "a string for a change"
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = 0

let b (x: int) (y: int) : int =
    printfn "doing b with %i %i" x y
    x + y

let c = "a string for a change"
"""

[<Test>]
let ``short let binding, two comments, short let binding`` () =
    formatSourceString
        false
        """
let a =  7.0
// some comment
// other comment
let b =   0.0908
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = 7.0
// some comment
// other comment
let b = 0.0908
"""

[<Test>]
let ``multiline expression, newline, short expression, short expression`` () =
    formatSourceString
        false
        """
asyncResult {
      let job =
        { JobType = EsriBoundaryImport
          FileToImport = filePath
          State = state
          DryRun = args.DryRun }

      importer.ApiMaster <! StartImportCmd job
      return Ok job
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
asyncResult {
    let job =
        { JobType = EsriBoundaryImport
          FileToImport = filePath
          State = state
          DryRun = args.DryRun }

    importer.ApiMaster <! StartImportCmd job
    return Ok job
}
"""

[<Test>]
let ``inner comment should make item multiline`` () =
    formatSourceString
        false
        """
    let a =
        // foo
        getA()
    return a
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    // foo
    getA ()

return a
"""

[<Test>]
let ``leading comment should not make item multiline`` () =
    formatSourceString
        false
        """
// You can also implement it via an object expression
let md' =
    { new MyDim }
printfn "DIM from C# but via Object Expression: %d" md'.Z
"""
        config
    |> prepend newline
    |> should
        equal
        """
// You can also implement it via an object expression
let md' = { new MyDim }
printfn "DIM from C# but via Object Expression: %d" md'.Z
"""

[<Test>]
let ``leading newline and comment should not make item multiline`` () =
    formatSourceString
        false
        """
printfn "DIM from C#: %d" md.Z

// You can also implement it via an object expression
let md' = { new MyDim }
printfn "DIM from C# but via Object Expression: %d" md'.Z
"""
        config
    |> prepend newline
    |> should
        equal
        """
printfn "DIM from C#: %d" md.Z

// You can also implement it via an object expression
let md' = { new MyDim }
printfn "DIM from C# but via Object Expression: %d" md'.Z
"""

[<Test>]
let ``multiple leading comments should keep short item short`` () =
    formatSourceString
        false
        """
// #if INTERACTIVE
// #else
#load "../FSharpx.TypeProviders/SetupTesting.fsx"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
"""
        config
    |> prepend newline
    |> should
        equal
        """
// #if INTERACTIVE
// #else
#load "../FSharpx.TypeProviders/SetupTesting.fsx"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
"""

[<Test>]
let ``comment after let binding does not make it multiline`` () =
    formatSourceString
        false
        """
let a = 7
let b = 8
// foo
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = 7
let b = 8
// foo
"""

[<Test>]
let ``existing blank line between multiline expressions`` () =
    formatSourceString
        false
        """
open Barry
printFn ()

open Foo
open Bar
"""
        config
    |> prepend newline
    |> should
        equal
        """
open Barry
printFn ()

open Foo
open Bar
"""

[<Test>]
let ``items should be collected from nested let-or-use in sequentials`` () =
    formatSourceString
        false
        """
let blah<'a> config : Type =
//#if DEBUG
        failwith ""
//#endif
        DoThing.doIt ()
        let result = Runner.Run<'a> config
        ()
"""
        { config with
            SpaceBeforeUppercaseInvocation = true
            SpaceBeforeClassConstructor = true
            SpaceBeforeMember = true
            SpaceBeforeColon = true
            SpaceBeforeSemicolon = true
            MultilineBlockBracketsOnSameColumn = true
            NewlineBetweenTypeDefinitionAndMembers = true
            AlignFunctionSignatureToIndentation = true
            AlternativeLongMemberDefinitions = true
            MultiLineLambdaClosingNewline = true }
    |> prepend newline
    |> should
        equal
        """
let blah<'a> config : Type =
    //#if DEBUG
    failwith ""
    //#endif
    DoThing.doIt ()
    let result = Runner.Run<'a> config
    ()
"""

[<Test>]
let ``leading newline because of trivia does not item multiline, 1709`` () =
    formatSourceString
        false
        """
[<Fact>]
let ``first lamba`` () =
    // You can use the `fun` keyword to write lambda's.
    // Mind the -> instead of C#'s =>
    // TODO: complete the lambda so that the value is returned in uppercase.
    let toUpperCase = fun a -> a
    // ref: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/lambda-expressions-the-fun-keyword

    let name = "Joey"
    let uppercased = toUpperCase name
    Assert.Equal("JOEY", uppercased)
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<Fact>]
let ``first lamba`` () =
    // You can use the `fun` keyword to write lambda's.
    // Mind the -> instead of C#'s =>
    // TODO: complete the lambda so that the value is returned in uppercase.
    let toUpperCase = fun a -> a
    // ref: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/functions/lambda-expressions-the-fun-keyword

    let name = "Joey"
    let uppercased = toUpperCase name
    Assert.Equal("JOEY", uppercased)
"""

[<Test>]
let ``leading multiline block comment followed by newline should not make item multiline, 1718`` () =
    formatSourceString
        false
        """
(*
    My personal favorite: Discriminated Unions!
    This is a feature related to sum types in category theory and incredibly useful for code correctness.
    ref: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/discriminated-unions
*)

type Days = Days of int  // you can alias types in F# to get a DDD kind of vibe.
type StoryPoints = StoryPoints of int
type Money = Money of double
"""
        config
    |> prepend newline
    |> should
        equal
        """
(*
    My personal favorite: Discriminated Unions!
    This is a feature related to sum types in category theory and incredibly useful for code correctness.
    ref: https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/discriminated-unions
*)

type Days = Days of int // you can alias types in F# to get a DDD kind of vibe.
type StoryPoints = StoryPoints of int
type Money = Money of double
"""

[<Test>]
let ``leading comment followed by newlines should not make item multiline`` () =
    formatSourceString
        false
        """
//



let a = 0
let b =  p
"""
        config
    |> prepend newline
    |> should
        equal
        """
//



let a = 0
let b = p
"""

[<Test>]
let ``define surrounded by newlines should not make item multiline`` () =
    formatSourceString
        false
        """
let a = p

#if MEH
()
#endif

let i = 00
let y = p
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = p

#if MEH
()
#endif

let i = 00
let y = p
"""

[<Test>]
let ``two short application followed by trailing block comment, 1538`` () =
    formatSourceString
        false
        """
printfn "%s" @"c:\def\ghi\jkl"
printfn "%s" "c:\\def\\ghi\\jkl"

(*
xyz
*)
"""
        config
    |> prepend newline
    |> should
        equal
        """
printfn "%s" @"c:\def\ghi\jkl"
printfn "%s" "c:\\def\\ghi\\jkl"

(*
xyz
*)
"""
