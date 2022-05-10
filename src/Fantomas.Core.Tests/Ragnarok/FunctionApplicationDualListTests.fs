module Fantomas.Core.Tests.Ragnarok.FunctionApplicationDualListTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

let config =
    { config with
        MultilineBlockBracketsOnSameColumn = true
        Ragnarok = true }

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
     with
     | ex -> printfn "meh")
    c [
        a1 // hey
        a2
    ] [
        b1 // comment
        b2
    ]
"""
