module Fantomas.Tests.NumberOfItemsMultilineInfixOperatorExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper
open Fantomas.FormatConfig

let config =
    { config with
          MultilineInfixMultilineFormatter = NumberOfItems }

[<Test>]
let ``simple infix operator expressions`` () =
    formatSourceString false """
let WebApp =
    route "/ping" >=> authorized >=> text "pong"

x
|> aVeryLongFunctionNameThatGoesOnAndOnForeverAlmost

x |> f |> g
    """ config
    |> prepend newline
    |> should equal """
let WebApp =
    route "/ping" >=> authorized >=> text "pong"

x |> aVeryLongFunctionNameThatGoesOnAndOnForeverAlmost

x
|> f
|> g
"""

[<Test>]
let ``single multiline infix operator and expression is longer than max width`` () =
    formatSourceString false """
x |> aVeryLongFunctionNameThatGoesOnAndOnForeverAndEverAndEver
"""  { config with MaxLineLength = 60 }
    |> prepend newline
    |> should equal """
x
|> aVeryLongFunctionNameThatGoesOnAndOnForeverAndEverAndEver
"""

[<Test>]
let ``multiple pipe boolean expression`` () =
    formatSourceString false """let result = a && b |>  f |>  g |>   h
"""  config
    |> prepend newline
    |> should equal """
let result =
    a
    && b
       |> f
       |> g
       |> h
"""

[<Test>]
let ``mathematical expressions`` () =
    formatSourceString false """
let w = x + y + z
let w = x - y - z
    """ config
    |> prepend newline
    |> should equal """
let w = x + y + z
let w = x - y - z
"""

[<Test>]
let ``equal sign operator should not move to next line`` () =
    formatSourceString false """let result =
(typ.GetInterface(typeof<System.Collections.IEnumerable>.FullName) = null)
"""  config
    |> prepend newline
    |> should equal """
let result =
    (typ.GetInterface(typeof<System.Collections.IEnumerable>.FullName) = null)
"""

[<Test>]
let ``always put pipe on next line`` () =
    formatSourceString false """
let a = List.init 40 (fun i -> generateThing i a) |> List.map mapThingToOtherThing
let b = 42 |> printfn "%i"
"""
        { config with
              MaxNewlineInfixOperatorExpressionNumberOfItems = 0 }
    |> prepend newline
    |> should equal """
let a =
    List.init 40 (fun i -> generateThing i a)
    |> List.map mapThingToOtherThing

let b =
    42
    |> printfn "%i"
"""

[<Test>]
let ``all newline infix operators`` () =
    formatSourceString false """
let composed = a >> b >> c
let piped = d |> e |> f
let tuplePiped = (g, h) ||> func |> printfn "%A"
let triplePiped = (i,j,k) |||> func |> printfn "%A"
let bound = l >>= m >>= n
"""  config
    |> prepend newline
    |> should equal """
let composed =
    a
    >> b
    >> c

let piped =
    d
    |> e
    |> f

let tuplePiped =
    (g, h)
    ||> func
    |> printfn "%A"

let triplePiped =
    (i, j, k)
    |||> func
    |> printfn "%A"

let bound =
    l
    >>= m
    >>= n
"""
