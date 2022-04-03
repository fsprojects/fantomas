module Fantomas.Tests.OpenTypeTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``open type in implementation`` () =
    formatSourceString
        false
        """
open type System.Math

let x = Min(1.0, 2.0)

module M =
    type DU = A | B | C

    let someOtherFunction x = x + 1

// Open only the type inside the module
open type M.DU

printfn "%A" A
"""
        config
    |> prepend newline
    |> should
        equal
        """
open type System.Math

let x = Min(1.0, 2.0)

module M =
    type DU =
        | A
        | B
        | C

    let someOtherFunction x = x + 1

// Open only the type inside the module
open type M.DU

printfn "%A" A
"""

[<Test>]
let ``open type in signature file`` () =
    formatSourceString
        true
        """
namespace MySigFile

open type System.Math
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace MySigFile

open type System.Math
"""
