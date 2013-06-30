module Fantomas.Tests.CompilerDirectiveTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

[<Test>]
let ``hash directives``() =
    formatSourceString false """
    #r "Fantomas.Tests.dll"
    #load "CodeFormatterTests.fs"
    """ config
    |> prepend newline
    |> append newline
    |> should equal """
#r "Fantomas.Tests.dll"
#load "CodeFormatterTests.fs"
"""

[<Test>]
let ``should keep compiler directives``() =
    formatSourceString false """
#if INTERACTIVE
#load "../FSharpx.TypeProviders/SetupTesting.fsx"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
#load "__setup__.fsx"
#endif
"""  config
    |> should equal """
#if INTERACTIVE
#load "../FSharpx.TypeProviders/SetupTesting.fsx"

SetupTesting.generateSetupScript __SOURCE_DIRECTORY__

#load "__setup__.fsx"
#endif"""

[<Test>]
let ``line, file and path identifiers``() =
    formatSourceString false """
    let printSourceLocation() =
        printfn "Line: %s" __LINE__
        printfn "Source Directory: %s" __SOURCE_DIRECTORY__
        printfn "Source File: %s" __SOURCE_FILE__
    printSourceLocation()
    """ config
    |> prepend newline
    |> should equal """
let printSourceLocation() = 
    printfn "Line: %s" __LINE__
    printfn "Source Directory: %s" __SOURCE_DIRECTORY__
    printfn "Source File: %s" __SOURCE_FILE__

printSourceLocation()"""

[<Test>]
let ``should keep #if, #else and #endif on compiler directives``() =
    formatSourceString false """
let x = 1
#if SILVERLIGHT
let useHiddenInitCode = false
#else
let useHiddenInitCode = true
#endif
let y = 2
"""  config
    |> prepend newline
    |> should equal """
let x = 1
#if SILVERLIGHT

let useHiddenInitCode = false
#else
let useHiddenInitCode = true
#endif
let y = 2"""

[<Test>]
let ``should handle nested compiler directives``() =
    formatSourceString false """
let [<Literal>] private assemblyConfig =
    #if DEBUG
    #if TRACE
    let x = 0
    "DEBUG;TRACE"
    #else
    "DEBUG"
    #endif
    #else
    #if TRACE
    "TRACE"
    #else
    ""
    #endif
    #endif
"""  config
    |> prepend newline
    |> should equal """
[<Literal>]
let private assemblyConfig =
#if DEBUG

#if TRACE
 
    let x = 0
    "DEBUG;TRACE"
#else
    "DEBUG"
#endif
#else
#if TRACE
    "TRACE"
#else
    ""
#endif
#endif"""