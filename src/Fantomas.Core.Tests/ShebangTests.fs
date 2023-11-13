module Fantomas.Core.Tests.ShebangTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``should keep a shebang, 2367`` () =
    let codeSnippet =
        """#!/usr/bin/env -S dotnet fsi
// random licensing stuff

open System

printfn "the best thing we've ever done"
"""

    formatSourceString codeSnippet config
    |> should
        equal
        """#!/usr/bin/env -S dotnet fsi
// random licensing stuff

open System

printfn "the best thing we've ever done"
"""
