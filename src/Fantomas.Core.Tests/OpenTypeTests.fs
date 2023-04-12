module Fantomas.Core.Tests.OpenTypeTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

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

[<Test>]
let ``trivia before open type inside open list, 2704`` () =
    formatSourceString
        false
        """
namespace CounterApp

open Fabulous
open Fabulous.XamarinForms

open type Fabulous.XamarinForms.View
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace CounterApp

open Fabulous
open Fabulous.XamarinForms

open type Fabulous.XamarinForms.View
"""
