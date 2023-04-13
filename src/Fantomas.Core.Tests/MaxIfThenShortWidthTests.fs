module Fantomas.Core.Tests.MaxIfThenShortWidthTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

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
        { config with MaxIfThenShortWidth = 12 }
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
        { config with
            MaxIfThenShortWidth = 100 }
    |> prepend newline
    |> should
        equal
        """
if // comment makes expr multiline
    a
then
    b
"""

[<Test>]
let ``apply same rules for nested if/then/else without else expr`` () =
    formatSourceString
        false
        """
if a then b
elif c then d
elif e then f
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then
    b
elif c then
    d
elif e then
    f
"""

[<Test>]
let ``apply same rules for nested if/then/else without else expr, MaxIfThenShortWidth = 15`` () =
    formatSourceString
        false
        """
if a then b
elif c then d
elif e then f
"""
        { config with MaxIfThenShortWidth = 15 }
    |> prepend newline
    |> should
        equal
        """
if a then b
elif c then d
elif e then f
"""
