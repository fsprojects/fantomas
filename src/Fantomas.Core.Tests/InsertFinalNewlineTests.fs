module Fantomas.Core.Tests.InsertFinalNewlineTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

let config =
    { config with
        InsertFinalNewline = false }

[<Test>]
let ``respect insert_final_newline = false`` () =
    formatSourceString
        """
let a =    0
"""
        config
    |> should equal "let a = 0"

[<Test>]
let ``respect insert_final_newline = false when last line contains trivia`` () =
    formatSourceString
        """
let mode =
    #if DEBUG
        "dev"
    #else
        "prod"
    #endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
let mode =
#if DEBUG
    "dev"
#else
    "prod"
#endif"""
