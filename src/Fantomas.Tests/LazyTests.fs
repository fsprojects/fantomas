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
    |> prepend newline
    |> should equal """
let v = // <- Lazy "1"
    lazy (1 |> string)
"""

[<Test>]
let ``lazy should not wrap with () for multiline``() =
    formatSourceString false """
let v = // <- Lazy "1"
    lazy
        "123456798123456798123456798"
        |> idLongFunctionThing
        |> string""" config
    |> prepend newline
    |> should equal """
let v = // <- Lazy "1"
    lazy
        "123456798123456798123456798"
        |> idLongFunctionThing
        |> string
"""

[<Test>]
let ``short lazy with parens and infix should keep parens`` () =
    formatSourceString false """let result = lazy (x + 10)"""  config
    |> prepend newline
    |> should equal """
let result = lazy (x + 10)
"""