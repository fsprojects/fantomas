module Fantomas.Tests.KeepNewlineAfterTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let configKNA = { config with KeepNewlineAfter = true }

[<Test>]
let ``keep newline after equal in let binding`` () =
    formatSourceString false """let a =
printfn "foo: %d"   42
"""  configKNA
    |> prepend newline
    |> should equal """
let a =
    printfn "foo: %d" 42
"""