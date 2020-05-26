module Fantomas.Tests.MaxBindingWidthTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let config = { config with MaxBindingWidth = 20 }

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
let ``should not apply to short member value definition``() =
    formatSourceString false """type T =
    let a = b + 1
    member this.c = d + 2
    """ { config with MaxBindingWidth = 30 }
    |> should equal """type T =
    let a = b + 1
    member this.c = d + 2
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

    member this.cccccccccccccc dddddddddddddd =
        dddddddddddddd + 2
"""

[<Test>]
let ``should not apply to short member function definition``() =
    formatSourceString false """type T =
    let a b = b + 1
    member this.c d = d + 2
    """ { config with MaxBindingWidth = 30 }
    |> should equal """type T =
    let a b = b + 1
    member this.c d = d + 2
"""