module Fantomas.Core.Tests.LazyTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``lazy should wrap with ()`` () =
    formatSourceString
        false
        """
let v = // <- Lazy "1"
    lazy
        1 |> string"""
        config
    |> prepend newline
    |> should
        equal
        """
let v = // <- Lazy "1"
    lazy (1 |> string)
"""

[<Test>]
let ``lazy should not wrap with () for multiline`` () =
    formatSourceString
        false
        """
let v = // <- Lazy "1"
    lazy
        "123456798123456798123456798"
        |> idLongFunctionThing
        |> string"""
        { config with
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
let v = // <- Lazy "1"
    lazy
        "123456798123456798123456798"
        |> idLongFunctionThing
        |> string
"""

[<Test>]
let ``short lazy with parens and infix should keep parens`` () =
    formatSourceString false """let result = lazy (x + 10)""" config
    |> prepend newline
    |> should
        equal
        """
let result = lazy (x + 10)
"""

[<Test>]
let ``multiline lazy with parenthesis and letOrUse expression, 1271`` () =
    formatSourceString
        false
        """
let setup =
  lazy
   (let thing = Thing()
    thing.DoSomething()
    let value = 1
    value)"""
        config
    |> prepend newline
    |> should
        equal
        """
let setup =
    lazy
        (let thing = Thing()
         thing.DoSomething()
         let value = 1
         value)
"""

[<Test>]
let ``comment after lazy keyword`` () =
    formatSourceString
        false
        """
lazy // comment
    foobar
"""
        config
    |> prepend newline
    |> should
        equal
        """
lazy // comment
    foobar
"""

[<Test>]
let ``lazy with long indent expr should not get any additional parenthesis`` () =
    formatSourceString
        false
        """
let theme = lazy Application.Current.RequestedTheme
"""
        config
    |> prepend newline
    |> should
        equal
        """
let theme = lazy Application.Current.RequestedTheme
"""
