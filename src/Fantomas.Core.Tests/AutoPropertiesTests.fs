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
type X() =
    member val Y: int = 7 with public get, private set
"""

[<Test>]
let ``plain get, private set`` () =
    formatSourceString
        """
type X() =
    member val Y: int = 7 with get, private set
"""
        config
    |> prepend newline
    |> should
        equal
        """
type X() =
    member val Y: int = 7 with get, private set
"""

[<Test>]
let ``internal get, plain set`` () =
    formatSourceString
        """
type X() =
    member val Y: int = 7 with internal get,  set
"""
        config
    |> prepend newline
    |> should
        equal
        """
type X() =
    member val Y: int = 7 with internal get, set
"""

[<Test>]
let ``public get, private set in signature`` () =
    formatSignatureString
        """
module A

type X =
    new: unit -> X
    member internal Y: int with public get, private set
"""
        config
    |> prepend newline
    |> should
        equal
        """
module A

type X =
    new: unit -> X
    member internal Y: int with public get, private set
"""
