module Fantomas.Core.Tests.NullnessTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``abstract property`` () =
    formatSourceString
        """
[<AbstractClass>]
type AbstractBase() =
    abstract Property1 : string | null with get, set
"""
        config
    |> prepend newline
    |> should
        equal
        """

"""
