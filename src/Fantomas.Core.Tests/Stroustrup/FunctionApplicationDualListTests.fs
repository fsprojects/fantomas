module Fantomas.Core.Tests.Stroustrup.FunctionApplicationDualListTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper
open Fantomas.Core

let config =
    { config with
        MultilineBracketStyle = Stroustrup }

[<Test>]
let ``two short lists`` () =
    formatSourceString
        false
        """
fn [ a1; a2 ]    [ b1 ; b2 ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
fn [ a1; a2 ] [ b1; b2 ]
"""

[<Test>]
let ``first list short, second multiline`` () =
    formatSourceString
        false
        """
fn [ a1
     a2 ] [
    b1 // comment
    b2
]
"""
        config
    |> prepend newline
    |> should
        equal
        """
fn [ a1; a2 ] [
    b1 // comment
    b2
]
"""

[<Test>]
let ``first list multiline, second multiline`` () =
    formatSourceString
        false
        """
fn [ a1 // hey
     a2 ] [
    b1 // comment
    b2
]
"""
        config
    |> prepend newline
    |> should
        equal
        """
fn [
    a1 // hey
    a2
] [
    b1 // comment
    b2
]
"""

[<Test>]
let ``first list multiline, second multiline and other long arguments`` () =
    formatSourceString
        false
        """
fn a b (try somethingDangerous with ex -> printfn "meh" ) c [ a1 // hey
                                                              a2 ] [
    b1 // comment
    b2
]
"""
        { config with MaxArrayOrListWidth = 0 }
    |> prepend newline
    |> should
        equal
        """
fn
    a
    b
    (try
        somethingDangerous
     with ex ->
         printfn "meh")
    c [
        a1 // hey
        a2
    ] [
        b1 // comment
        b2
    ]
"""

[<Test>]
let ``two long lists as parameters, 2681`` () =
    formatSourceString
        false
        """
Layout.twoColumnLayoutWithStyles
    styles
    [
        element1 longParameterName1 param2 param3
        element1 longParameterName1 param2 param3
    ] 
    [
        element1 longParameterName1 param2 param3
        element1 longParameterName1 param2 param3
    ]
"""
        { config with MaxArrayOrListWidth = 0 }
    |> prepend newline
    |> should
        equal
        """
Layout.twoColumnLayoutWithStyles styles [
    element1 longParameterName1 param2 param3
    element1 longParameterName1 param2 param3
] [
    element1 longParameterName1 param2 param3
    element1 longParameterName1 param2 param3
]
"""
