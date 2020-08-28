module Fantomas.Tests.DotGetTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``a TypeApp inside a DotGet should stay on the same line, 994`` () =
    formatSourceString false """
Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<option<option<unit>>>.GetGenericTypeDefinition().MakeGenericType(t)).Assembly
"""  config
    |> prepend newline
    |> should equal """
Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<option<option<unit>>>.GetGenericTypeDefinition()
        .MakeGenericType(t)).Assembly
"""
