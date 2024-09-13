module Fantomas.Core.Tests.AutoPropertiesTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``public get, private set`` () =
    formatSourceString
        """
type X() =
    member val Y: int = 7 with public get, private set
"""
        config
    |> prepend newline
    |> should
        equal
        """

"""

[<Test>]
let ``public get, private set in signature`` () =
    formatSignatureString
        """
module A

type X() =
    member val internal Y: int = 7 with public get, private set
"""
        config
    |> prepend newline
    |> should
        equal
        """

"""
