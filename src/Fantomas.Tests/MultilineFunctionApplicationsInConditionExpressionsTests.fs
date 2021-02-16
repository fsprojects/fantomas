module Fantomas.Tests.MultilineFunctionApplicationsInConditionExpressionsTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``inside match expression, 1403`` () =
    formatSourceString
        false
        """
let foo () =
    match b.TryGetValue (longlonglonglonglong, b) with
    | true, i -> Some i
    | false, _ -> failwith ""
"""
        { config with MaxLineLength = 40 }
    |> prepend newline
    |> should
        equal
        """
let foo () =
    match
        b.TryGetValue
            (
                longlonglonglonglong,
                b
            )
        with
    | true, i -> Some i
    | false, _ -> failwith ""
"""
