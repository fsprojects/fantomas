module Fantomas.Core.Tests.Stroustrup.SynExprAnonRecdStructTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper
open Fantomas.Core

let config =
    { config with
        MultilineBracketStyle = Stroustrup }

[<Test>]
let ``anonymous struct record with trivia`` () =
    formatSourceString
        false
        """
struct // 1
    {| // 2
        // 3
        X = 4
    // 5       
    |} // 6 
"""
        config
    |> prepend newline
    |> should
        equal
        """
struct // 1
    {| // 2
        // 3
        X = 4
    // 5
    |} // 6
"""
