module Fantomas.Core.Tests.DallasTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

[<Test>]
let ``proof of concept`` () =
    formatSourceString
        false
        """
let a =   1
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = 1
"""

[<Test>]
let ``named module with let binding`` () =
    formatSourceString
        false
        """
module A.B
let a =   1
"""
        config
    |> prepend newline
    |> should
        equal
        """
module A.B

let a = 1
"""
