module Fantomas.Tests.ContextTests

open NUnit.Framework
open FsUnit
open Fantomas.Context
open Fantomas.Tests.TestHelper

[<Test>]
let ``sepSpace should not add an additional space if the line ends with a space`` () =
    let expr = !- "let a = " +> sepSpace
    let result = dump (expr Context.Default)
    result |> should equal "let a = "