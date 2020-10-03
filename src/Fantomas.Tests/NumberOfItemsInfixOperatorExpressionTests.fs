module Fantomas.Tests.NumberOfItemsInfixOperatorExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper
open Fantomas.FormatConfig

let config =
    { config with
          InfixOperatorExpressionMultilineFormatter = NumberOfItems }

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
let ``should not add newline before = operator after |>`` () =
    formatSourceString false """1 |> max 0 = 1""" config
    |> should equal """1 |> max 0 = 1
"""

[<Test>]
let ``should add space around .. .. operators and not add newline`` () =
    formatSourceString false """[10 .. -1 .. 1]""" config
    |> should equal """[ 10 .. -1 .. 1 ]
"""

[<Test>]
let ``long expression with pipe should be single line`` () =
    formatSourceString false "let a = List.init 40 (fun i -> generateThing i a) |> List.map mapThingToOtherThing" config
    |> prepend newline
    |> should equal """
let a =
    List.init 40 (fun i -> generateThing i a) |> List.map mapThingToOtherThing
"""

[<Test>]
let ``modulo operator on same line, 780`` () =
    formatSourceString false """let hasUnEvenAmount regex line = (Regex.Matches(line, regex).Count - Regex.Matches(line, "\\\\" + regex).Count) % 2 = 1
"""  config
    |> prepend newline
    |> should equal """
let hasUnEvenAmount regex line =
    (Regex.Matches(line, regex).Count - Regex.Matches(line, "\\\\" + regex).Count) % 2 = 1
"""

[<Test>]
let ``parameter after multiline string, 783`` () =
    formatSourceString false "
let ``match bang`` () =
    formatSourceString false \"\"\"
async {
    match! myAsyncFunction() with
    | Some x -> printfn \"%A\" x
    | None -> printfn \"Function returned None!\"
}\"\"\"   config
    |> prepend newline
    |> should equal \"\"\"
async {
    match! myAsyncFunction () with
    | Some x -> printfn \"%A\" x
    | None -> printfn \"Function returned None!\"
}
\"\"\"
"    config
    |> prepend newline
    |> should equal "
let ``match bang`` () =
    formatSourceString false \"\"\"
async {
    match! myAsyncFunction() with
    | Some x -> printfn \"%A\" x
    | None -> printfn \"Function returned None!\"
}\"\"\" config
    |> prepend newline
    |> should equal \"\"\"
async {
    match! myAsyncFunction () with
    | Some x -> printfn \"%A\" x
    | None -> printfn \"Function returned None!\"
}
\"\"\"
"

[<Test>]
let ``combining lines breaks function precedence 488`` () =
    formatSourceString false """fun () -> ()
|> Some
"""  config
    |> prepend newline
    |> should equal """
fun () -> ()
|> Some
"""

[<Test>]
let ``function with LPAREN_STAR_RPAREN`` () =
    formatSourceString false """
let private distanceBetweenTwoPoints (latA, lngA) (latB, lngB) =
    if latA = latB && lngA = lngB then
        0.
    else
        let theta = lngA - lngB

        let dist =
            Math.Sin(deg2rad (latA))
            * Math.Sin(deg2rad (latB))
            + (Math.Cos(deg2rad (latA))
               * Math.Cos(deg2rad (latB))
               * Math.Cos(deg2rad (theta)))
            |> Math.Acos
            |> rad2deg
            |> (*) (60. * 1.1515 * 1.609344)

        dist
"""  config
    |> prepend newline
    |> should equal """
let private distanceBetweenTwoPoints (latA, lngA) (latB, lngB) =
    if latA = latB && lngA = lngB then
        0.
    else
        let theta = lngA - lngB

        let dist =
            Math.Sin(deg2rad (latA)) * Math.Sin(deg2rad (latB))
            + (Math.Cos(deg2rad (latA))
               * Math.Cos(deg2rad (latB))
               * Math.Cos(deg2rad (theta)))
            |> Math.Acos
            |> rad2deg
            |> (*) (60. * 1.1515 * 1.609344)

        dist
"""

[<Test>]
let ``keep comment after or operator, 1095`` () =
    formatSourceString false """
    let f x =
        a
        || // other case
        match n with
            | 17 -> false
            | _ -> true
            """ config
    |> prepend newline
    |> should equal """
let f x =
    a
    || // other case
    match n with
    | 17 -> false
    | _ -> true
"""

[<Test>]
let ``simple math in one line`` () =
    formatSourceString false """let myValue = a + b * c
"""  config
    |> prepend newline
    |> should equal """
let myValue = a + b * c
"""

[<Test>]
let ``simple math reversed`` () =
    formatSourceString false """let myValue = a * b + c
"""  config
    |> prepend newline
    |> should equal """
let myValue = a * b + c
"""

[<Test>]
let ``multiple sum operators`` () =
    formatSourceString false """let myValue = a + b * c + d
"""  config
    |> prepend newline
    |> should equal """
let myValue = a + b * c + d
"""

[<Test>]
let ``nested math sample`` () =
    formatSourceString false """
        let dist =
            aaaaaaaaaaaaaaaaaaaaaaaa
            * bbbbbbbbbbbbbbbbbbbbbbbbb
            + (ccccccccccccccccccccccccc
               * ddddddddddddddddddddddd
               * eeeeeeeeeeeeeeeeeeeeeee)
"""  config
    |> prepend newline
    |> should equal """
let dist =
    aaaaaaaaaaaaaaaaaaaaaaaa * bbbbbbbbbbbbbbbbbbbbbbbbb
    + (ccccccccccccccccccccccccc
       * ddddddddddddddddddddddd
       * eeeeeeeeeeeeeeeeeeeeeee)
"""

[<Test>]
let ``split infix operators according to nested structure in AST, 988`` () =
    formatSourceString false """
let shouldIncludeRelationship relName =
    req.Includes |> List.exists (fun path ->
      path.Length >= currentIncludePath.Length + 1
      && path |> List.take (currentIncludePath.Length + 1) = currentIncludePath @ [relName]
    )
"""  config
    |> prepend newline
    |> should equal """
let shouldIncludeRelationship relName =
    req.Includes
    |> List.exists (fun path ->
        path.Length >= currentIncludePath.Length + 1
        && path |> List.take (currentIncludePath.Length + 1) = currentIncludePath @ [ relName ])
"""
