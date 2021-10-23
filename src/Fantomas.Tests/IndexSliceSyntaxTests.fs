module Fantomas.Tests.IndexSliceSyntaxTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``don't convert index syntax without dot to application`` () =
    formatSourceString
        false
        """
expr1[expr2]
"""
        config
    |> prepend newline
    |> should
        equal
        """
expr1[expr2]
"""

[<Test>]
let ``slicing examples`` () =
    formatSourceString
        false
        """
let arr = [| 1;2;3 |]
arr[0] <- 2
arr[0]
arr[0..1]
arr[..1]
arr[0..]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let arr = [| 1; 2; 3 |]
arr[0] <- 2
arr[0]
arr[0..1]
arr[..1]
arr[0..]
"""

[<Test>]
let ``higher-dimensional arrays`` () =
    formatSourceString
        false
        """
let arr = Array4D.create 3 4 5 6 0
arr[0,2,3,4] <- 2
arr[0,2,3,4]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let arr = Array4D.create 3 4 5 6 0
arr[0, 2, 3, 4] <- 2
arr[0, 2, 3, 4]
"""