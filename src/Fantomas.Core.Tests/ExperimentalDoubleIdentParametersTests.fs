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

[<Test>]
let ``sample in long member`` () =
    formatSourceString
        """
type X() =
    member x.Y(a:int, b:int, c:int, d:int, e:int) =
    
        // Long body goes here...
        longBody
"""
        config
    |> prepend newline
    |> should
        equal
        """
type X() =
    member x.Y
            (
                a:
                    int,
                b:
                    int,
                c:
                    int,
                d:
                    int,
                e:
                    int
            ) =

        // Long body goes here...
        longBody
"""
