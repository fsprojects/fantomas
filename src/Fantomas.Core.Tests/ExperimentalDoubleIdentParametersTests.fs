module Fantomas.Core.Tests.ExperimentalDoubleIdentParametersTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

let config =
    { config with
        ExperimentalDoubleIndentParameters = true
        MaxLineLength = 10 }

[<Test>]
let ``initial sample`` () =
    formatSourceString
        """
let sillyfuncWithParams parameterName1 ignoredParameterName2 ignoredParameterName3 =
    longBody
"""
        config
    |> prepend newline
    |> should
        equal
        """
let sillyfuncWithParams
        parameterName1
        ignoredParameterName2
        ignoredParameterName3
        =
    longBody
"""
