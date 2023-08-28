module Fantomas.Core.Tests.DotLambdaTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``function call`` () =
    formatSourceString
        false
        """
let x = "a" |> _.ToString()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = "a" |> _.ToString()
"""

[<Test>]
let ``property call`` () =
    formatSourceString
        false
        """
let x = "a" |> _.Length
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = "a" |> _.Length
"""

[<Test>]
let ``property of method invocation`` () =
    formatSourceString
        false
        """
let c = _.ToString().Length
"""
        config
    |> prepend newline
    |> should
        equal
        """
let c = _.ToString().Length
"""

[<Test>]
let ``property of function invocation`` () =
    formatSourceString
        false
        """
let c = _.foo().Length
"""
        config
    |> prepend newline
    |> should
        equal
        """
let c = _.foo().Length
"""
