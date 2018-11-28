module Fantomas.Tests.DynamicOperatorTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``Keep () when dynamic operator is used``() =
    formatSourceString false "let memoEquals x = x?(k + 1)" config
    |> should equal """let memoEquals x = x?(k + 1)
"""

[<Test>]
let ``Remove () when dynamic operator is string``() =
    formatSourceString false "let memoEquals x = x?k" config
    |> should equal """let memoEquals x = x?k
"""
