module Fantomas.Tests.MaxFunctionBindingWidthTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let config = { config with MaxFunctionBindingWidth = 20 }

[<Test>]
let ``should apply to function definition``() =
    formatSourceString false """let a bbbbbbbbbbbbbbbbbbbbbbbbbb = bbbbbbbbbbbbbbbbbbbbbbbbbb + 1""" config
    |> should equal """let a bbbbbbbbbbbbbbbbbbbbbbbbbb =
    bbbbbbbbbbbbbbbbbbbbbbbbbb + 1
"""

[<Test>]
let ``should not apply to short function definition``() =
    formatSourceString false """let a b = b + 1""" config
    |> should equal """let a b = b + 1
"""

[<Test>]
let ``should apply to member function definition``() =
    formatSourceString false """type T =
    let aaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbb = bbbbbbbbbbbbbbbbbbb + 1
    member this.cccccccccccccc dddddddddddddd = dddddddddddddd + 2
    """ config
    |> should equal """type T =
    let aaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbb =
        bbbbbbbbbbbbbbbbbbb + 1

    member this.cccccccccccccc dddddddddddddd = dddddddddddddd + 2
"""

[<Test>]
let ``should apply to typed member function definition``() =
    formatSourceString false """type T =
    let aaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbb = bbbbbbbbbbbbbbbbbbb + 1
    member this.cccccccccccccc dddddddddddddd: int = dddddddddddddd + 2
    """ config
    |> should equal """type T =
    let aaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbb =
        bbbbbbbbbbbbbbbbbbb + 1

    member this.cccccccccccccc dddddddddddddd: int = dddddddddddddd + 2
"""

[<Test>]
let ``should not apply to short member function definition``() =
    formatSourceString false """type T =
    let a b = b + 1
    member this.c d = d + 2
    """ { config with MaxFunctionBindingWidth = 30 }
    |> should equal """type T =
    let a b = b + 1
    member this.c d = d + 2
"""
