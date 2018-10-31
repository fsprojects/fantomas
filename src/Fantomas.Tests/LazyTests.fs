module Fantomas.Tests.LazyTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``lazy should wrap with ()``() =
    formatSourceString false """
let v = // <- Lazy "1"
    lazy
        1 |> string""" config
    |> fun x -> x
    |> prepend newline
    |> should equal """
let v = // <- Lazy "1"
    lazy (1 |> string)
"""