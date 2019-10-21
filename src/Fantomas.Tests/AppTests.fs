module Fantomas.Tests.AppTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

// the current behavior results in a compile error since the |> is merged to the last line 
[<Test>]
let ``no nln before lambda #503``() =
    formatSourceString false """
let a =
    b
    |> List.exists (fun p ->
        p.a && p.b |> List.exists (fun o -> o.a = "lorem ipsum dolor sit amet"))
    """ { config with PageWidth = 80 }
    |> prepend newline
    |> should equal """
let a =
    b
    |> List.exists (fun p ->
        p.a && p.b |> List.exists (fun o -> o.a = "lorem ipsum dolor sit amet"))
"""
