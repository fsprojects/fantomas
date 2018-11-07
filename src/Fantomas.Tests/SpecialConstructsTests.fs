module Fantomas.Tests.SpecialConstructsTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``embedded IL``() =
    formatSourceString false """
let inline private retype<'T, 'U> (x : 'T) : 'U = (# "" x : 'U #)""" config
    |> prepend newline
    |> should equal """
let inline private retype<'T, 'U> (x : 'T) : 'U = (# "" x : 'U #)
"""
