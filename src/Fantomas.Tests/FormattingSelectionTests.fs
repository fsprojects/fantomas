module Fantomas.Tests.FormattingSelectionTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

[<Test>]
let ``should format a part of a line correctly``() =
    formatSelectionFromString false (makeRange 3 8 3 11) """
let x = 2 + 3
let y = 1+2
let z = x + y""" config
    |> should equal """
let x = 2 + 3
let y = 1 + 2
let z = x + y"""

[<Test>]
let ``should format a whole line correctly and preserve indentation``() =
    formatSelectionFromString false (makeRange 3 4 3 34) """
    let base1 = d1 :> Base1
    let derived1 = base1 :?> Derived1""" config
    |> should equal """
    let base1 = d1 :> Base1
    let derived1 = base1 :?> Derived1"""

[<Test>]
let ``should format a few lines correctly and preserve indentation``() =
    formatSelectionFromString false (makeRange 3 5 5 52) """
let rangeTest testValue mid size =
    match testValue with
    | var1 when var1 >= mid - size/2 && var1 <= mid + size/2 -> printfn "The test value is in range."
    | _ -> printfn "The test value is out of range."

let (var1, var2) as tuple1 = (1, 2)""" config
    |> should equal """
let rangeTest testValue mid size =
    match testValue with
    | var1 when var1 >= mid - size / 2 && var1 <= mid + size / 2 -> 
        printfn "The test value is in range."
    | _ -> printfn "The test value is out of range."

let (var1, var2) as tuple1 = (1, 2)"""

[<Test>]
let ``should format a top-level let correctly``() =
    formatSelectionFromString false (makeRange 3 0 3 11) """
let x = 2 + 3
let y = 1+2
let z = x + y""" config
    |> should equal """
let x = 2 + 3
let y = 1 + 2
let z = x + y"""