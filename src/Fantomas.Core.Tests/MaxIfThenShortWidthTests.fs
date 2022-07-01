module Fantomas.Core.Tests.MaxIfThenShortWidthTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

[<Test>]
let ``default behavior of MaxIfThenShortWidth`` () =
    formatSourceString
        false
        """
if a then () 
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then
    ()
"""

[<Test>]
let ``keep entire expression in one line`` () =
    formatSourceString
        false
        """
if a then () 
"""
        { config with MaxIfThenShortWidth = 3 }
    |> prepend newline
    |> should
        equal
        """
if a then ()
"""

[<Test>]
let ``always put then on next line if the ifExpr is multiline`` () =
    formatSourceString
        false
        """
if // comment makes expr multiline
   a then b
"""
        { config with MaxIfThenShortWidth = 100 }
    |> prepend newline
    |> should
        equal
        """
if // comment makes expr multiline
    a
then
    b
"""
