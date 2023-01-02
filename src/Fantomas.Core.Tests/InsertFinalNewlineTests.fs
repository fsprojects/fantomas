module Fantomas.Core.Tests.InsertFinalNewlineTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

let config =
    { config with
        InsertFinalNewline = false }

[<Test>]
let ``respect insert_final_newline = false`` () =
    formatSourceString
        false
        """
let a =    0
"""
        config
    |> should equal "let a = 0"

[<Test>]
let ``respect insert_final_newline = false when last line contains trivia`` () =
    formatSourceString
        false
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
