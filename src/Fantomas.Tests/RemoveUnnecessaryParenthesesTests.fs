module Fantomas.Tests.RemoveUnnecessaryParenthesesTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``parentheses around single identifiers in if expressions are unnecessary, 684`` () =
    formatSourceString
        false
        """
if (foo) then bar else baz
"""
        config
    |> prepend newline
    |> should
        equal
        """
if foo then bar else baz
"""
