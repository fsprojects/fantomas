module Fantomas.Tests.MaxValueBindingWidthTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let config = { config with MaxValueBindingWidth = 20; }

[<Test>]
let ``should apply to value definition``() =
    formatSourceString false """let a = bbbbbbbbbbbbbbbbbbbbbbbbbb""" config
    |> should equal """let a =
    bbbbbbbbbbbbbbbbbbbbbbbbbb
"""

[<Test>]
let ``should not apply to short value definition``() =
    formatSourceString false """let a = b""" config
    |> should equal """let a = b
"""

[<Test>]
let ``should apply to member value definition``() =
    formatSourceString false """type T =
    let aaaaaaaaaaaaaaaaaaaa = bbbbbbbbbbbbbbbbbbb + 1
    member this.ccccccccccccccccccccccccccccccc = dddddddddddddddddddddddddddd + 2
    """ config
    |> should equal """type T =
    let aaaaaaaaaaaaaaaaaaaa =
        bbbbbbbbbbbbbbbbbbb + 1

    member this.ccccccccccccccccccccccccccccccc =
        dddddddddddddddddddddddddddd + 2
"""

[<Test>]
let ``should apply to typed member value definition``() =
    formatSourceString false """type T =
    let aaaaaaaaaaaaaaaaaaaa = bbbbbbbbbbbbbbbbbbb + 1
    member (this.ccccccccccccccccccccccccccccccc: int)= dddddddddddddddddddddddddddd + 2
    """ config
    |> should equal """type T =
    let aaaaaaaaaaaaaaaaaaaa =
        bbbbbbbbbbbbbbbbbbb + 1

    member (this.ccccccccccccccccccccccccccccccc: int) =
        dddddddddddddddddddddddddddd + 2
"""

[<Test>]
let ``should not apply to short member value definition``() =
    formatSourceString false """type T =
    let a = b + 1
    member this.c = d + 2
    """ { config with MaxFunctionBindingWidth = 30 }
    |> should equal """type T =
    let a = b + 1
    member this.c = d + 2
"""
