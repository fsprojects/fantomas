module Fantomas.Core.Tests.IdentTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

[<Test>]
let ``type with ticks, 2164`` () =
    formatSourceString
        false
        """
let foo = {| ``type`` = "hi" |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let foo = {| ``type`` = "hi" |}
"""

[<Test>]
let ``include with ticks, 2167`` () =
    formatSourceString
        false
        """
            match req.``include`` with
            | None -> tc.TestItems()
            | Some includedTests -> includedTests.ToArray()
"""
        config
    |> prepend newline
    |> should
        equal
        """
match req.``include`` with
| None -> tc.TestItems()
| Some includedTests -> includedTests.ToArray()
"""
