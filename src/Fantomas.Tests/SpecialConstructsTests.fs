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
let inline private retype<'T, 'U> (x: 'T): 'U = (# "" x : 'U #)
"""

[<Test>]
let ``don't add whitespace in chained accessors, 566`` () =
    formatSourceString false """type F =
  abstract G : int list -> Map<int, int>

let x : F = { new F with member __.G _ = Map.empty }
x.G[].TryFind 3
"""  ({ config with
          SpaceAfterComma = false
          SpaceAfterSemicolon = false
          SpaceAroundDelimiter = false })
    |> prepend newline
    |> should equal """
type F =
    abstract G: int list -> Map<int,int>

let x: F =
    {new F with
        member __.G _ = Map.empty}

x.G[].TryFind 3
"""